{- -*- haskell -*- -}
module Subversion.FileSystem.Revision
    ( Rev

    , withRevision

    , getRevisionProp
    , getRevisionPropList
    , setRevisionProp

    , getNodeHistory
    )
    where

import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as B8
import           Foreign.C.String
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Subversion.Error
import           Subversion.FileSystem
import           Subversion.FileSystem.Root
import           Subversion.Hash
import           Subversion.Pool
import           Subversion.String
import           Subversion.Types
import           System.IO.Unsafe


{- Monad Rev ----------------------------------------------------------------- -}

newtype Rev a = Rev { unRev :: ReaderT FileSystemRoot IO a }

instance Functor Rev where
    fmap f c = Rev (fmap f (unRev c))

instance Monad Rev where
    c >>= f = Rev (unRev c >>= unRev . f)
    return  = Rev . return
    fail    = Rev . fail

instance MonadFS Rev where
    getRoot        = Rev ask
    unsafeIOToFS a = Rev (liftIO a)
    isTransaction  = Rev (return False)


{- functions and types ------------------------------------------------------- -}

data SVN_FS_HISTORY_T


foreign import ccall unsafe "svn_fs_revision_root"
        _revision_root :: Ptr (Ptr SVN_FS_ROOT_T) -> Ptr SVN_FS_T -> SVN_REVNUM_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_revision_prop"
        _revision_prop :: Ptr (Ptr SVN_STRING_T) -> Ptr SVN_FS_T -> SVN_REVNUM_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_revision_proplist"
        _revision_proplist :: Ptr (Ptr APR_HASH_T) -> Ptr SVN_FS_T -> SVN_REVNUM_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_change_rev_prop"
        _change_rev_prop :: Ptr SVN_FS_T -> SVN_REVNUM_T -> CString -> Ptr SVN_STRING_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_node_history"
        _node_history :: Ptr (Ptr SVN_FS_HISTORY_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_history_prev"
        _history_prev :: Ptr (Ptr SVN_FS_HISTORY_T) -> Ptr SVN_FS_HISTORY_T -> SVN_BOOLEAN_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_history_location"
        _history_location :: Ptr CString -> Ptr SVN_REVNUM_T -> Ptr SVN_FS_HISTORY_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


getRevisionRoot :: FileSystem -> Int -> IO FileSystemRoot
getRevisionRoot fs revNum
    = do pool <- newPool
         alloca $ \ rootPtrPtr ->
             withFSPtr fs $ \ fsPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 do svnErr $ _revision_root rootPtrPtr fsPtr (fromIntegral revNum) poolPtr
                    -- root は pool にも fs にも依存する。
                    wrapFSRoot (touchPool pool >> touchFS fs)
                        =<< peek rootPtrPtr


withRevision :: FileSystem -> Int -> Rev a -> IO a
withRevision fs revNum c
    = getRevisionRoot fs revNum
      >>= runReaderT (unRev c)


getRevisionProp :: FileSystem -> Int -> String -> IO (Maybe String)
getRevisionProp fs revNum name
    = do pool <- newPool
         alloca $ \ valPtrPtr ->
             withFSPtr   fs   $ \ fsPtr   ->
             withCString name $ \ namePtr ->
             withPoolPtr pool $ \ poolPtr ->
             do svnErr $ _revision_prop valPtrPtr fsPtr (fromIntegral revNum) namePtr poolPtr
                prop <- peekSvnString' =<< peek valPtrPtr
                -- prop は pool の中から讀み取られるので、それが濟むま
                -- で pool が死んでは困る。
                touchPool pool
                return $ fmap B8.unpack prop


getRevisionPropList :: FileSystem -> Int -> IO [(String, String)]
getRevisionPropList fs revNum
    = do pool <- newPool
         alloca $ \ hashPtrPtr ->
             withFSPtr   fs   $ \ fsPtr   ->
             withPoolPtr pool $ \ poolPtr ->
             do svnErr $ _revision_proplist hashPtrPtr fsPtr (fromIntegral revNum) poolPtr
                hash <- wrapHash (touchPool pool) =<< peek hashPtrPtr
                mapHash' (\ (n, v)
                              -> peekSvnString v
                                 >>=
                                 return . ((,) n) . B8.unpack) hash


setRevisionProp :: FileSystem -> Int -> String -> Maybe String -> IO ()
setRevisionProp fs revNum name valStr
    = do pool <- newPool
         let value = fmap B8.pack valStr
         withFSPtr fs $ \ fsPtr ->
             withCString    name  $ \ namePtr  ->
             withSvnString' value $ \ valuePtr ->
             withPoolPtr    pool  $ \ poolPtr  ->
             svnErr $ _change_rev_prop fsPtr (fromIntegral revNum) namePtr valuePtr poolPtr


getNodeHistory :: Bool -> FilePath -> Rev [(FilePath, Int)]
getNodeHistory crossCopies path
    = do pool <- unsafeIOToFS $ newPool
         root <- getRoot
         unsafeIOToFS $ alloca $ \ histPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             do svnErr $ _node_history histPtrPtr rootPtr pathPtr poolPtr
                lazyReadHist pool =<< peek histPtrPtr
    where
      lazyReadHist pool histPtr = unsafeInterleaveIO $ readHist pool histPtr

      readHist :: Pool -> Ptr SVN_FS_HISTORY_T -> IO [(FilePath, Int)]
      readHist pool histPtr
          = alloca           $ \ histPtrPtr ->
            withPoolPtr pool $ \ poolPtr    ->
            do svnErr $ _history_prev histPtrPtr histPtr (marshalBool crossCopies) poolPtr
               got <- peek histPtrPtr

               if got == nullPtr then
                   -- ヒストリの終端に達した。これ以後、Pool は解放され
                   -- ても構はない。
                   touchPool pool >> return []
                 else
                   do x  <- getHistLocation got pool
                      xs <- lazyReadHist pool got
                      return (x:xs)

      getHistLocation :: Ptr SVN_FS_HISTORY_T -> Pool -> IO (FilePath, Int)
      getHistLocation histPtr pool
          = alloca           $ \ pathPtrPtr ->
            alloca           $ \ revNumPtr  ->
            withPoolPtr pool $ \ poolPtr    ->
            do svnErr $ _history_location pathPtrPtr revNumPtr histPtr poolPtr
               path   <- peekCString           =<< peek pathPtrPtr
               revNum <- return . fromIntegral =<< peek revNumPtr
               return (path, revNum)