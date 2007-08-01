{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.FileSystem
    ( FileSystem -- private
    , SVN_FS_T -- private

    , wrapFS -- private
    , withFSPtr -- private
    , touchFS -- private

    , fsVersion

    , createFileSystem
    , fsTypeBDB
    , fsTypeFSFS

    , openFileSystem
    , deleteFileSystem
    , hotCopyFileSystem

    , getFileSystemType
    , getFileSystemPath

    , getYoungestRev
    )
    where

import           Control.Monad
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.Error
import           Subversion.Hash
import           Subversion.Pool
import           Subversion.Types


newtype FileSystem = FileSystem (ForeignPtr SVN_FS_T)
data SVN_FS_T

foreign import ccall "svn_fs_version"
        _version :: IO (Ptr SVN_VERSION_T)

foreign import ccall "svn_fs_create"
        _create :: Ptr (Ptr SVN_FS_T) -> CString -> Ptr APR_HASH_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall "svn_fs_open"
        _open :: Ptr (Ptr SVN_FS_T) -> CString -> Ptr APR_HASH_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall "svn_fs_delete_fs"
        _delete_fs :: CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall "svn_fs_hotcopy"
        _hotcopy :: CString -> CString -> SVN_BOOLEAN_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall "svn_fs_type"
        _type :: Ptr CString -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall "svn_fs_path"
        _path :: Ptr SVN_FS_T -> Ptr APR_POOL_T -> IO CString

foreign import ccall "svn_fs_youngest_rev"
        _youngest_rev :: Ptr SVN_REVNUM_T -> Ptr SVN_FS_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapFS :: IO () -> Ptr SVN_FS_T -> IO FileSystem
wrapFS finalizer fsPtr
    = do fs <- newForeignPtr_ fsPtr
         GF.addForeignPtrConcFinalizer fs finalizer
         return $ FileSystem fs


withFSPtr :: FileSystem -> (Ptr SVN_FS_T -> IO a) -> IO a
withFSPtr (FileSystem fs) = withForeignPtr fs


touchFS :: FileSystem -> IO ()
touchFS (FileSystem fs) = touchForeignPtr fs


fsVersion :: IO Version
fsVersion = _version >>= peekVersion


fsTypeBDB :: String
fsTypeBDB = (#const_str SVN_FS_TYPE_BDB)


fsTypeFSFS :: String
fsTypeFSFS = (#const_str SVN_FS_TYPE_FSFS)


createFileSystem :: FilePath -> [(String, String)] -> IO FileSystem
createFileSystem path configPairs
    = do pool <- newPool
         alloca $ \ fsPtrPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 do config <- pairsToHash configPairs
                    svnErr $ _create fsPtrPtr pathPtr (unsafeHashToPtr config) poolPtr

                    fs <- wrapFS (touchPool pool) =<< peek fsPtrPtr

                    -- config には fs が死ぬまでは生きてゐて慾しい。
                    GF.addForeignPtrConcFinalizer (case fs of FileSystem x -> x)
                          $ touchHash config

                    return fs


openFileSystem :: FilePath -> [(String, String)] -> IO FileSystem
openFileSystem path configPairs
    = do pool <- newPool
         alloca $ \ fsPtrPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             do config <- pairsToHash configPairs
                svnErr $ _open fsPtrPtr pathPtr (unsafeHashToPtr config) poolPtr

                fs <- wrapFS (touchPool pool) =<< peek fsPtrPtr

                -- config には fs が死ぬまでは生きてゐて慾しい。
                GF.addForeignPtrConcFinalizer (case fs of FileSystem x -> x)
                      $ touchHash config

                return fs


deleteFileSystem :: FilePath -> IO ()
deleteFileSystem path
    = do pool <- newPool
         withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _delete_fs pathPtr poolPtr


hotCopyFileSystem :: FilePath -> FilePath -> Bool -> IO ()
hotCopyFileSystem src dest clean
    = do pool <- newPool
         withCString src $ \ srcPath ->
             withCString dest $ \ destPath ->
             withPoolPtr pool $ \ poolPtr  ->
             svnErr $ _hotcopy srcPath destPath (marshalBool clean) poolPtr


getFileSystemType :: FilePath -> IO String
getFileSystemType path
    = do pool <- newPool
         alloca $ \ typePtrPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             do svnErr $ _type typePtrPtr pathPtr poolPtr
                t <- peekCString =<< peek typePtrPtr
                return t


getFileSystemPath :: FileSystem -> IO String
getFileSystemPath fs
    = do pool <- newPool
         withFSPtr fs $ \ fsPtr ->
             withPoolPtr pool $ \ poolPtr ->
             do path <- peekCString =<< _path fsPtr poolPtr
                touchPool pool
                return path


getYoungestRev :: FileSystem -> IO Int
getYoungestRev fs
    = do pool <- newPool
         alloca $ \ revNumPtr ->
             withFSPtr   fs   $ \ fsPtr   ->
             withPoolPtr pool $ \ poolPtr ->
             do svnErr $ _youngest_rev revNumPtr fsPtr poolPtr
                return . fromIntegral =<< peek revNumPtr
