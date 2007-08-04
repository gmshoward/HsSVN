{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.FileSystem.Root
    ( MonadFS(..)

    , FileSystemRoot -- private
    , SVN_FS_ROOT_T -- private

    , wrapFSRoot -- private
    , withFSRootPtr -- private

    , getRootFS -- private

    , getFileLength
    , getFileMD5

    , getFileContents
    , getFileContentsLBS

    , getNodeProp
    , getNodePropList

    , getDirEntries
    , getPathsChanged

    , checkPath
    , isDirectory
    , isFile
    )
    where

import           Control.Monad
import           Data.ByteString.Base
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.FileSystem
import           Subversion.FileSystem.DirEntry
import           Subversion.FileSystem.PathChange
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


foreign import ccall unsafe "svn_fs_root_fs"
        _root_fs :: Ptr SVN_FS_ROOT_T -> IO (Ptr SVN_FS_T)

foreign import ccall unsafe "svn_fs_file_length"
        _file_length :: Ptr SVN_FILESIZE_T -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_file_md5_checksum"
        _file_md5_checksum :: Ptr CUChar -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_file_contents"
        _file_contents :: Ptr (Ptr SVN_STREAM_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_node_proplist"
        _node_proplist :: Ptr (Ptr APR_HASH_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_node_prop"
        _node_prop :: Ptr (Ptr SVN_STRING_T) -> Ptr SVN_FS_ROOT_T -> CString -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_dir_entries"
        _dir_entries :: Ptr (Ptr APR_HASH_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_paths_changed"
        _paths_changed :: Ptr (Ptr APR_HASH_T) -> Ptr SVN_FS_ROOT_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

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


getRootFS :: FileSystemRoot -> IO FileSystem
getRootFS root
    = withFSRootPtr root $ \ rootPtr ->
      -- 實際には root が生きてゐる限り fs は死なないのだが、念の爲。
      wrapFS (touchFSRoot root) =<< _root_fs rootPtr


getFileLength :: MonadFS m => FilePath -> m Integer
getFileLength path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ lenPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             do svnErr $ _file_length lenPtr rootPtr pathPtr poolPtr
                return . toInteger =<< peek lenPtr


getFileMD5 :: MonadFS m => FilePath -> m [Word8]
getFileMD5 path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ allocaArray md5Len $ \ bufPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             do svnErr $ _file_md5_checksum bufPtr rootPtr pathPtr poolPtr
                return . map fromIntegral =<< peekArray md5Len bufPtr
    where
      md5Len = (#const APR_MD5_DIGESTSIZE)


-- FIXME: String を返す事についての迷ひを doc に書く
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
             >>= wrapHash (touchFSRoot root >> touchPool pool)
             >>= hashToValues


getPathsChanged :: MonadFS m => m [(FilePath, PathChange)]
getPathsChanged
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ hashPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             do svnErr $ _paths_changed hashPtrPtr rootPtr poolPtr
                peek hashPtrPtr
                     >>= wrapHash (touchFSRoot root >> touchPool pool)
                     >>= hashToPairs


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