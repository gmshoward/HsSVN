{- -*- haskell -*- -}

-- #prune

-- |An interface to the Subversion filesystem.

#include "HsSVN.h"

module Subversion.FileSystem
    ( -- * Type
      FileSystem
    , SVN_FS_T -- private

    , wrapFS -- private
    , withFSPtr -- private
    , touchFS -- private

      -- * Information of the libsvn_fs itself
    , fsVersion

      -- * Filesystem creation, opening and destruction
    , createFileSystem
    , fsConfigFSType
    , fsTypeBDB
    , fsTypeFSFS

    , openFileSystem
    , deleteFileSystem
    , hotCopyFileSystem

      -- * Accessors
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

-- |@'FileSystem'@ is an opaque object representing a Subversion
-- filesystem.
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

-- |@fsVersion@ returns a version information of the libsvn_fs.
fsVersion :: IO Version
fsVersion = _version >>= peekVersion


-- |@'fsConfigFSType'@ is a config key to specify the filesystem
-- back-end.
fsConfigFSType :: String
fsConfigFSType = (#const_str SVN_FS_CONFIG_FS_TYPE)

-- |@'fsTypeBDB'@ is a config value representing the Berkeley-DB
-- back-end.
fsTypeBDB :: String
fsTypeBDB = (#const_str SVN_FS_TYPE_BDB)

-- |@'fsTypeFSFS'@ is a config value representing the
-- Native-filesystem back-end.
fsTypeFSFS :: String
fsTypeFSFS = (#const_str SVN_FS_TYPE_FSFS)

-- |@'createFileSystem'@ creates a new, empty Subversion
-- filesystem. Note that creating a raw filesystem is different from
-- creating a repository. If you want a new repository, use
-- 'Subversion.Repository.createRepository' instead.
createFileSystem
    :: FilePath           -- ^ Where to create the filesystem. The
                          --   path most not currently exist, but its
                          --   parent must exist.
    -> [(String, String)] -- ^ A list of @(key, value)@ tuples which
                          --   modifies the behavior of the
                          --   filesystem. The interpretation of it is
                          --   specific to the filesystem back-end.
                          -- 
                          --   If the list contains a value for
                          --   'fsConfigFSType', that value determines
                          --   the filesystem type for the new
                          --   filesystem. Currently defined values
                          --   are:
                          --
                          --   ['fsTypeBDB'] Berkeley-DB implementation
                          --
                          --   ['fsTypeFSFS'] Native-filesystem
                          --   implementation
                          --
                          --   If the list does not contain a value
                          --   for 'fsConfigFSType' then the default
                          --   filesystem type will be used. This will
                          --   typically be BDB for version 1.1 and
                          --   FSFS for later versions, though the
                          --   caller should not rely upon any
                          --   particular default if they wish to
                          --   ensure that a filesystem of specific
                          --   type is created.
    -> IO FileSystem      -- ^ The new filesystem.
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

-- |@'openFileSystem'@ opens a Subversion filesystem. Note that you
-- probably don't want to use this directly. Take a look at
-- 'Subversion.Repository.openRepository' instead.
openFileSystem
    :: FilePath           -- ^ Where the filesystem located on.
    -> [(String, String)] -- ^ A list of @(key, value)@ tuples which
                          --   modifies the behavior of the
                          --   filesystem. The interpretation of it is
                          --   specific to the filesystem back-end.
    -> IO FileSystem
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

-- |@'deleteFileSystem'@ deletes a Subversion filesystem. Note that
-- you probably don't want to use this directly. Take a look at
-- 'Subversion.Repository.deleteRepository' instead.
deleteFileSystem :: FilePath -> IO ()
deleteFileSystem path
    = do pool <- newPool
         withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _delete_fs pathPtr poolPtr

-- |@'hotCopyFileSystem'@ copies a possibly live Subversion filesystem
-- from one location to another.
hotCopyFileSystem :: FilePath -- ^ Source
                  -> FilePath -- ^ Destination
                  -> Bool     -- ^ If this is true,
                              --   @'hotCopyFileSystem'@ performs
                              --   cleanup on the source filesystem as
                              --   part of the copy opeation;
                              --   currently, this means deleting
                              --   copied, unused logfiles for a
                              --   Berkeley DB source filesystem.
                  -> IO ()
hotCopyFileSystem src dest clean
    = do pool <- newPool
         withCString src $ \ srcPath ->
             withCString dest $ \ destPath ->
             withPoolPtr pool $ \ poolPtr  ->
             svnErr $ _hotcopy srcPath destPath (marshalBool clean) poolPtr

-- |@'getFileSystemPath' fsPath@ returns a string identifying the
-- back-end type of the Subversion filesystem located on @fsPath@. The
-- string should be equal to one of the @fsType*@ defined constants,
-- unless the filesystem is a new back-end type added in a later
-- version of Subversion.
--
-- In general, the type should make no difference in the filesystem's
-- semantics, but there are a few situations (such as backups) where
-- it might matter.
getFileSystemType :: FilePath -> IO String
getFileSystemType path
    = do pool <- newPool
         alloca $ \ typePtrPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             do svnErr $ _type typePtrPtr pathPtr poolPtr
                t <- peekCString =<< peek typePtrPtr
                return t

-- |@'getFileSystemPath' fs@ returns the path to @fs@'s
-- repository. Note that this is what was passed to 'createFileSystem'
-- or 'openFileSystem'; might be absolute, might not.
getFileSystemPath :: FileSystem -> IO FilePath
getFileSystemPath fs
    = do pool <- newPool
         withFSPtr fs $ \ fsPtr ->
             withPoolPtr pool $ \ poolPtr ->
             do path <- peekCString =<< _path fsPtr poolPtr
                touchPool pool
                return path

-- |@'getYoungestRev' fs@ returns the number of the youngest revision
-- in filesystem @fs@. The oldest revision in any filesystem is
-- numbered zero.
getYoungestRev :: FileSystem -> IO RevNum
getYoungestRev fs
    = do pool <- newPool
         alloca $ \ revNumPtr ->
             withFSPtr   fs   $ \ fsPtr   ->
             withPoolPtr pool $ \ poolPtr ->
             do svnErr $ _youngest_rev revNumPtr fsPtr poolPtr
                return . fromIntegral =<< peek revNumPtr
