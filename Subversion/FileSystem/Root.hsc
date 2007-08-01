{- -*- haskell -*- -}
module Subversion.FileSystem.Root
    ( MonadFS(..)

    , FileSystemRoot -- private
    , SVN_FS_ROOT_T -- private

    , wrapFSRoot -- private
    , withFSRootPtr -- private

    , getFileContents
    , getFileContentsLBS

    , getNodeProp
    , getNodePropList

    , getDirEntries

    , checkPath
    , isDirectory
    , isFile
    )
    where

import           Control.Monad
import           Data.ByteString.Base
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.FileSystem.DirEntry
import           Subversion.Error
import           Subversion.Hash
import           Subversion.Pool
import           Subversion.Stream
import           Subversion.String
import           Subversion.Types


{- class MonadFS ------------------------------------------------------------- -}

class Monad m => MonadFS m where
    getRoot       :: m FileSystemRoot -- private
    unsafeIOToFS  :: IO a -> m a
    isTransaction :: m Bool


{- functions and types ------------------------------------------------------- -}

newtype FileSystemRoot = FileSystemRoot (ForeignPtr SVN_FS_ROOT_T)
data SVN_FS_ROOT_T


foreign import ccall unsafe "svn_fs_file_contents"
        _file_contents :: Ptr (Ptr SVN_STREAM_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_node_proplist"
        _node_proplist :: Ptr (Ptr APR_HASH_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_node_prop"
        _node_prop :: Ptr (Ptr SVN_STRING_T) -> Ptr SVN_FS_ROOT_T -> CString -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_dir_entries"
        _dir_entries :: Ptr (Ptr APR_HASH_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_check_path"
        _check_path :: Ptr SVN_NODE_KIND_T -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_is_dir"
        _is_dir :: Ptr SVN_BOOLEAN_T -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_is_file"
        _is_file :: Ptr SVN_BOOLEAN_T -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapFSRoot :: IO () -> Ptr SVN_FS_ROOT_T -> IO FileSystemRoot
wrapFSRoot finalizer rootPtr
    = do root <- newForeignPtr_ rootPtr
         GF.addForeignPtrConcFinalizer root finalizer
         return $ FileSystemRoot root


withFSRootPtr :: FileSystemRoot -> (Ptr SVN_FS_ROOT_T -> IO a) -> IO a
withFSRootPtr (FileSystemRoot root) = withForeignPtr root


touchFSRoot :: FileSystemRoot -> IO ()
touchFSRoot (FileSystemRoot root) = touchForeignPtr root


getFileContents :: MonadFS m => FilePath -> m String
getFileContents path
    = liftM L8.unpack $ getFileContentsLBS path


-- もしこれが txn-root ならば遲延ストリーム讀出しが安全でなくなるので、
-- ファイル全體を正格に讀んでしまふ。
getFileContentsLBS :: MonadFS m => FilePath -> m LazyByteString
getFileContentsLBS path
    = do root  <- getRoot
         isTxn <- isTransaction
         pool  <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ ioPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
                 withCString path $ \ pathPtr ->
                     withPoolPtr pool $ \ poolPtr ->
                         (svnErr $ _file_contents ioPtrPtr rootPtr pathPtr poolPtr)
                         >>  peek ioPtrPtr
                         >>= wrapStream (touchPool pool)
                         >>= (if isTxn then
                                  sStrictReadLBS
                              else
                                  sReadLBS)


getNodePropList :: MonadFS m => FilePath -> m [(String, String)]
getNodePropList path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ hashPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             do svnErr $ _node_proplist hashPtrPtr rootPtr pathPtr poolPtr
                hash <- wrapHash (touchPool pool) =<< peek hashPtrPtr
                mapHash' (\ (n, v)
                              -> peekSvnString v
                                 >>=
                                 return . ((,) n) . B8.unpack) hash


getNodeProp :: MonadFS m => FilePath -> String -> m (Maybe String)
getNodeProp path name
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ valPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withCString   name $ \ namePtr ->
             withPoolPtr   pool $ \ poolPtr ->
                 do svnErr $ _node_prop valPtrPtr rootPtr pathPtr namePtr poolPtr
                    prop <- peekSvnString' =<< peek valPtrPtr
                    -- prop は pool の中から讀み取られるので、それが濟
                    -- むまで pool が死んでは困る。
                    touchPool pool
                    return $ fmap B8.unpack prop


getDirEntries :: MonadFS m => FilePath -> m [DirEntry]
getDirEntries path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ hashPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             (svnErr $ _dir_entries hashPtrPtr rootPtr pathPtr poolPtr)
             >>  peek hashPtrPtr
             >>= wrapHash (touchFSRoot root)
             >>= hashToValues


checkPath :: MonadFS m => FilePath -> m NodeKind
checkPath path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ kindPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
                 do svnErr $ _check_path kindPtr rootPtr pathPtr poolPtr
                    return . unmarshalNodeKind =<< peek kindPtr


isDirectory :: MonadFS m => FilePath -> m Bool
isDirectory path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ boolPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
                 do svnErr $ _is_dir boolPtr rootPtr pathPtr poolPtr
                    return . unmarshalBool =<< peek boolPtr


isFile :: MonadFS m => FilePath -> m Bool
isFile path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ boolPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
                 do svnErr $ _is_file boolPtr rootPtr pathPtr poolPtr
                    return . unmarshalBool =<< peek boolPtr