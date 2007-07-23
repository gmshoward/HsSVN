{- -*- haskell -*- -}
module Subversion.FileSystem.Root
    ( FileSystemRoot
    , SVN_FS_ROOT_T
    , getRevisionRoot

    , wrapFSRoot

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

import           Control.Monad
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


getFileContents :: FileSystemRoot -> FilePath -> IO String
getFileContents root path
    = liftM L8.unpack $ getFileContentsLBS root path


getFileContentsLBS :: FileSystemRoot -> FilePath -> IO LazyByteString
getFileContentsLBS root path
    = do pool <- newPool
         alloca $ \ ioPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
                 withCString path $ \ pathPtr ->
                     withPoolPtr pool $ \ poolPtr ->
                         (svnErr $ _file_contents ioPtrPtr rootPtr pathPtr poolPtr)
                         >>  peek ioPtrPtr
                         >>= wrapStream (touchPool pool)
                         >>= sReadLBS


applyText :: FileSystemRoot -> FilePath -> Maybe String -> String -> IO ()
applyText root path resultMD5 contents
    = applyTextLBS root path resultMD5 (L8.pack contents)


applyTextLBS :: FileSystemRoot -> FilePath -> Maybe String -> LazyByteString -> IO ()
applyTextLBS root path resultMD5 contents
    = do pool <- newPool
         alloca $ \ ioPtrPtr ->
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


makeFile :: FileSystemRoot -> FilePath -> IO ()
makeFile root path
    = do pool <- newPool
         withFSRootPtr root   $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _make_file rootPtr pathPtr poolPtr


makeDirectory :: FileSystemRoot -> FilePath -> IO ()
makeDirectory root path
    = do pool <- newPool
         withFSRootPtr root   $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _make_dir rootPtr pathPtr poolPtr


deleteEntry :: FileSystemRoot -> FilePath -> IO ()
deleteEntry root path
    = do pool <- newPool
         withFSRootPtr   root $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _delete rootPtr pathPtr poolPtr


getDirEntries :: FileSystemRoot -> FilePath -> IO [DirEntry]
getDirEntries root path
    = do pool <- newPool
         alloca $ \ hashPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             (svnErr $ _dir_entries hashPtrPtr rootPtr pathPtr poolPtr)
             >>  peek hashPtrPtr
             >>= wrapHash (touchFSRoot root)
             >>= hashToValues

