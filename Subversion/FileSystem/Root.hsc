{- -*- haskell -*- -}

{-# OPTIONS_HADDOCK prune #-}

-- |An interface to functions that work on both existing revisions and
-- ongoing transactions in a filesystem.

#include "HsSVN.h"

module Subversion.FileSystem.Root
    ( -- * Class
      MonadFS(..)

    , FileSystemRoot -- private
    , SVN_FS_ROOT_T -- private

    , wrapFSRoot -- private
    , withFSRootPtr -- private

    , getRootFS -- private

      -- * Getting file content and others
    , getFileLength
    , getFileMD5

    , getFileContents
    , getFileContentsLBS

      -- * Getting node type
    , checkPath
    , isDirectory
    , isFile

      -- * Getting node property
    , getNodeProp
    , getNodePropList

      -- * Getting nodes in directory
    , getDirEntries

      -- * Getting change list
    , getPathsChanged
    )
    where

import           Control.Monad
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as Lazy        (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8   hiding (ByteString)
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

-- |@'MonadFS' m@ is a monad which holds internally either an existing
-- revision or ongoing transaction.
class Monad m => MonadFS m where
    -- private
    getRoot       :: m FileSystemRoot

    -- |@'unsafeIOToFS'@ runs an IO monad in 'MonadFS'.
    -- 
    -- This is unsafe if the monad is a transaction monad. When a
    -- transaction fails before getting commited, all transactional
    -- operations are cancelled at once. But if you had launched
    -- missiles with 'unsafeIOToFS' during the transaction, your
    -- missiles can never go back. So you must use this carefully.
    unsafeIOToFS  :: IO a -> m a

    -- |@'isTransaction'@ returns True iff the monad is a transaction
    -- monad.
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

-- |@'getFileLength' path@ returns the length of file @path@.
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

-- |@'getFileMD5' path@ returns the MD5 checksum of file @path@. If
-- the filesystem does not have a prerecorded checksum for @path@, it
-- doesn't calculate a checksum dynamically, just puts all 0's into
-- the resulting digest. (By convention, the all-zero checksum is
-- considered to match any checksum.)
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


-- |@'getFileContents' path@ returns the content of file @path@.
--
-- If this is a non-transactional monad (that is, a monad whose
-- 'isTransaction' returns 'Prelude.False'), it reads the content
-- /lazilly/. But if this is a transactional monad, 'getFileContents'
-- does the operation /strictly/: I mean it loads the entire file onto
-- the memory! This is because @svn_fs_file_contents()@ in the
-- libsvn_fs doesn't guarantee, when we are in a transaction, that we
-- can progressively read a file which is suddenly modified in the
-- same transaction during the progressive reading.
--
-- I think 'getFileContents' might have to return
-- @['Data.Word.Word8']@ instead of 'Prelude.String' because every
-- files in filesystem should be considered binary. Yes,
-- 'Prelude.readFile' also returns 'Prelude.String' but this is an
-- immature part of Haskell. I wish someday the GHC gets more clear
-- distinction between binary data and an Unicode string, then I
-- change the form of 'getFileContents' alike.
getFileContents :: MonadFS m => FilePath -> m String
getFileContents path
    = liftM L8.unpack $ getFileContentsLBS path


-- |@'getFileContentsLBS'@ does the same thing as 'getFileContents'
-- but returns 'Data.ByteString.Lazy.ByteString' instead.
getFileContentsLBS :: MonadFS m => FilePath -> m Lazy.ByteString
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

-- |@'getNodePropList' path@ returns the property list of @path@ in a
-- revision or transaction.
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

-- |@'getNodeProp' path propName@ returns the value of the property
-- named @propName@ of @path@ in a revision or transaction.
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

-- |@'getDirEntries' path@ returns a list containing the entries of
-- the directory at @path@ in a revision or transaction.
getDirEntries :: MonadFS m => FilePath -> m [DirEntry]
getDirEntries path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ hashPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             do svnErr $ _dir_entries hashPtrPtr rootPtr pathPtr poolPtr
                peek hashPtrPtr
                    >>= wrapHash (touchFSRoot root >> touchPool pool)
                    >>= hashToValues

-- |@'getPathsChanged'@ returns a list containing descriptions of the
-- paths changed under a revision or transaction.
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

-- |@'checkPath' path@ returns a 'Subversion.Types.NodeKind' for
-- @path@. Unlike most other actions, 'checkPath' doesn't throw an
-- error even if the @path@ doesn't point to an existent node: in that
-- case it just returns 'Subversion.Types.NoNode'.
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

-- |@'isDirectory' path@ returns 'Prelude.True' iff the @path@ points
-- to a directory in a revision or transaction. It returns
-- 'Prelude.False' for inexistent path.
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

-- |@'isFile' path@ returns 'Prelude.True' iff the @path@ points to a
-- file in a revision or transaction. It returns 'Prelude.False' for
-- inexistent path.
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