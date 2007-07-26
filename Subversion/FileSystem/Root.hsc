{- -*- haskell -*- -}
module Subversion.FileSystem.Root
    ( MonadFS(runFS)
    , Rev
    , Txn

    , FileSystemRoot
    , SVN_FS_ROOT_T

    , wrapFSRoot

    , withRevision

    , getFileContents
    , getFileContentsLBS

    , applyText
    , applyTextLBS

    , makeFile
    , makeDirectory

    , deleteEntry

    , getDirEntries
    )
    where

import           Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.Trans as Tr
import           Data.ByteString.Base
import qualified Data.ByteString.Lazy.Char8 as L8
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.FileSystem
import           Subversion.FileSystem.DirEntry
import           Subversion.Error
import           Subversion.Hash
import           Subversion.Pool
import           Subversion.Stream
import           Subversion.Types


{- class MonadFS ------------------------------------------------------------- -}

class Monad m => MonadFS m where
    getRoot :: m FileSystemRoot
    liftIO  :: IO a -> m a
    runFS   :: m a -> FileSystemRoot -> IO a


{- Monad Rev ----------------------------------------------------------------- -}

newtype Rev a = Rev { unRev :: ReaderT FileSystemRoot IO a }

instance Functor Rev where
    fmap f c = Rev (fmap f (unRev c))

instance Monad Rev where
    c >>= f = Rev (unRev c >>= unRev . f)
    return  = Rev . return
    fail    = Rev . fail

instance MonadFS Rev where
    getRoot  = Rev ask
    liftIO a = Rev (Tr.liftIO a)
    runFS c  = runReaderT (unRev c)


{- Monad Txn ----------------------------------------------------------------- -}

newtype Txn a = Txn { unTxn :: ReaderT FileSystemRoot IO a }

instance Functor Txn where
    fmap f c = Txn (fmap f (unTxn c))

instance Monad Txn where
    c >>= f = Txn (unTxn c >>= unTxn . f)
    return  = Txn . return
    fail    = Txn . fail

instance MonadFS Txn where
    getRoot  = Txn ask
    liftIO a = Txn (Tr.liftIO a)
    runFS c  = runReaderT (unTxn c)


{- functions and types ------------------------------------------------------- -}

newtype FileSystemRoot = FileSystemRoot (ForeignPtr SVN_FS_ROOT_T)
data SVN_FS_ROOT_T


foreign import ccall unsafe "svn_fs_revision_root"
        _revision_root :: Ptr (Ptr SVN_FS_ROOT_T) -> Ptr SVN_FS_T -> SVN_REVNUM_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_file_contents"
        _file_contents :: Ptr (Ptr SVN_STREAM_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_apply_text"
        _apply_text :: Ptr (Ptr SVN_STREAM_T) -> Ptr SVN_FS_ROOT_T -> CString -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_make_file"
        _make_file :: Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_make_dir"
        _make_dir :: Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_dir_entries"
        _dir_entries :: Ptr (Ptr APR_HASH_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_delete"
        _delete :: Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapFSRoot :: IO () -> Ptr SVN_FS_ROOT_T -> IO FileSystemRoot
wrapFSRoot finalizer rootPtr
    = do root <- newForeignPtr_ rootPtr
         GF.addForeignPtrConcFinalizer root finalizer
         return $ FileSystemRoot root


withFSRootPtr :: FileSystemRoot -> (Ptr SVN_FS_ROOT_T -> IO a) -> IO a
withFSRootPtr (FileSystemRoot root) = withForeignPtr root


touchFSRoot :: FileSystemRoot -> IO ()
touchFSRoot (FileSystemRoot root) = touchForeignPtr root


getRevisionRoot :: FileSystem -> Int -> IO FileSystemRoot
getRevisionRoot fs revNum
    = do pool <- newPool
         alloca $ \ rootPtrPtr ->
             withFSPtr fs $ \ fsPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 (svnErr $ _revision_root rootPtrPtr fsPtr (fromIntegral revNum) poolPtr)
                 >>  peek rootPtrPtr
                 >>= (wrapFSRoot $
                      -- root は pool にも fs にも依存する。
                      touchPool pool >> touchFS fs)


withRevision :: FileSystem -> Int -> Rev a -> IO a
withRevision fs revNum c
    = getRevisionRoot fs revNum
      >>= runFS c


getFileContents :: MonadFS m => FilePath -> m String
getFileContents path
    = liftM L8.unpack $ getFileContentsLBS path


-- FIXME: もしこれが txn-root ならば遲延ストリーム讀出しが安全でなくな
-- るので、ファイル全體を正格に讀んでしまふ。
getFileContentsLBS :: MonadFS m => FilePath -> m LazyByteString
getFileContentsLBS path
    = do root <- getRoot
         pool <- liftIO newPool
         liftIO $ alloca $ \ ioPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
                 withCString path $ \ pathPtr ->
                     withPoolPtr pool $ \ poolPtr ->
                         (svnErr $ _file_contents ioPtrPtr rootPtr pathPtr poolPtr)
                         >>  peek ioPtrPtr
                         >>= wrapStream (touchPool pool)
                         >>= sReadLBS


applyText :: FilePath -> Maybe String -> String -> Txn ()
applyText path resultMD5 contents
    = applyTextLBS path resultMD5 (L8.pack contents)


applyTextLBS :: FilePath -> Maybe String -> LazyByteString -> Txn ()
applyTextLBS path resultMD5 contents
    = do root <- getRoot
         pool <- liftIO newPool
         liftIO $ alloca $ \ ioPtrPtr ->
             withFSRootPtr root      $ \ rootPtr      ->
             withCString   path      $ \ pathPtr      ->
             withCString'  resultMD5 $ \ resultMD5Ptr ->
             withPoolPtr   pool      $ \ poolPtr      ->
             do svnErr $ _apply_text ioPtrPtr rootPtr pathPtr resultMD5Ptr poolPtr
                io <- wrapStream (touchPool pool) =<< peek ioPtrPtr
                sWriteLBS io contents
                sClose io
    where
      withCString' :: Maybe String -> (CString -> IO a) -> IO a
      withCString' Nothing    f = f nullPtr
      withCString' (Just str) f = withCString str f


makeFile :: FilePath -> Txn ()
makeFile path
    = do root <- getRoot
         pool <- liftIO newPool
         liftIO $ withFSRootPtr root $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _make_file rootPtr pathPtr poolPtr


makeDirectory :: FilePath -> Txn ()
makeDirectory path
    = do root <- getRoot
         pool <- liftIO newPool
         liftIO $ withFSRootPtr root $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _make_dir rootPtr pathPtr poolPtr


deleteEntry :: FilePath -> Txn ()
deleteEntry path
    = do root <- getRoot
         pool <- liftIO newPool
         liftIO $ withFSRootPtr root $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _delete rootPtr pathPtr poolPtr


getDirEntries :: MonadFS m => FilePath -> m [DirEntry]
getDirEntries path
    = do root <- getRoot
         pool <- liftIO newPool
         liftIO $ alloca $ \ hashPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             (svnErr $ _dir_entries hashPtrPtr rootPtr pathPtr poolPtr)
             >>  peek hashPtrPtr
             >>= wrapHash (touchFSRoot root)
             >>= hashToValues

