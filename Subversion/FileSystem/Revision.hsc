{-# LANGUAGE
    DeriveDataTypeable
  , EmptyDataDecls
  , ForeignFunctionInterface
  #-}
-- |An interface to functions that work on an existing
-- (i.e. read-only) revision in a filesystem.

module Subversion.FileSystem.Revision
    ( -- * Type
      Rev

      -- * Running the monad
    , withRevision

      -- * Getting revision info
    , getRevisionNumber

      -- * Accessing revision property
    , getRevisionProp
    , getRevisionProp'
    , getRevisionPropList
    , getRevisionPropList'
    , setRevisionProp

      -- * Getting node history
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

-- |@'Rev' a@ is a FS monad which reads some data from an existing
-- revision and finally returns @a@. See
-- 'Subversion.FileSystem.Root.MonadFS'.
--
-- Since 'Rev' monad does no transactions,
-- 'Subversion.FileSystem.Root.unsafeIOToFS' isn't really unsafe. You
-- can do any I\/O actions in a monadic computation if you wish.
newtype Rev a = Rev { unRev :: ReaderT FileSystemRoot IO a }

instance Functor Rev where
    fmap f c = Rev (fmap f (unRev c))

instance Monad Rev where
    c >>= f = Rev (unRev c >>= unRev . f)
    return  = Rev . return
    fail    = Rev . fail

instance Applicative Rev where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance MonadFS Rev where
    getRoot        = Rev ask
    unsafeIOToFS a = Rev (liftIO a)
    isTransaction  = Rev (return False)


{- functions and types ------------------------------------------------------- -}

data SVN_FS_HISTORY_T


foreign import ccall unsafe "svn_fs_revision_root"
        _revision_root :: Ptr (Ptr SVN_FS_ROOT_T) -> Ptr SVN_FS_T -> SVN_REVNUM_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_revision_root_revision"
        _revision_root_revision :: Ptr SVN_FS_ROOT_T -> IO SVN_REVNUM_T

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
                    -- FS Root depends on both pool and fs.
                    wrapFSRoot (touchPool pool >> touchFS fs)
                        =<< peek rootPtrPtr

-- |@'withRevision'@ runs a 'Rev' monad in an IO monad.
withRevision :: FileSystem -> RevNum -> Rev a -> IO a
withRevision fs revNum c
    = getRevisionRoot fs revNum
      >>= runReaderT (unRev c)

-- |Return the revision number.
getRevisionNumber :: Rev RevNum
getRevisionNumber
    = do root <- getRoot
         unsafeIOToFS $ withFSRootPtr root $ \ rootPtr ->
             (fmap fromIntegral (_revision_root_revision rootPtr))

-- |@'getRevisionProp' propName@ returns the value of the property
-- named @propName@ of the revision.
getRevisionProp :: String -> Rev (Maybe String)
getRevisionProp name
    = do root   <- getRoot
         fs     <- unsafeIOToFS $ getRootFS root
         revNum <- getRevisionNumber
         unsafeIOToFS $ getRevisionProp' fs revNum name

-- |@'getRevisionProp'' fs revNum propName@ returns the value of the
-- property named @propName@ of revision @revNum@ of filesystem @fs@.
getRevisionProp' :: FileSystem -> RevNum -> String -> IO (Maybe String)
getRevisionProp' fs revNum name
    = do pool <- newPool
         alloca $ \ valPtrPtr ->
             withFSPtr   fs   $ \ fsPtr   ->
             withCString name $ \ namePtr ->
             withPoolPtr pool $ \ poolPtr ->
             do svnErr $ _revision_prop valPtrPtr fsPtr (fromIntegral revNum) namePtr poolPtr
                prop <- peekSvnString' =<< peek valPtrPtr
                -- We read prop in the pool so we don't want pool to
                -- be freed that time.
                touchPool pool
                return $ fmap B8.unpack prop

-- |Return the entire property list of the revision.
getRevisionPropList :: Rev [(String, String)]
getRevisionPropList
    = do root   <- getRoot
         fs     <- unsafeIOToFS $ getRootFS root
         revNum <- getRevisionNumber
         unsafeIOToFS $ getRevisionPropList' fs revNum

-- |Return the entire property list of the given revision.
getRevisionPropList' :: FileSystem -> RevNum -> IO [(String, String)]
getRevisionPropList' fs revNum
    = do pool <- newPool
         alloca $ \ hashPtrPtr ->
             withFSPtr   fs   $ \ fsPtr   ->
             withPoolPtr pool $ \ poolPtr ->
             do svnErr $ _revision_proplist hashPtrPtr fsPtr (fromIntegral revNum) poolPtr
                hash <- wrapHash (touchPool pool) =<< peek hashPtrPtr
                mapHash' (\ (n, v)
                              -> fmap ((,) n . B8.unpack) (peekSvnString v))
                         hash

-- |Change, add or delete a property on a revision. Note that revision
-- properties are non-historied: you can change them after the
-- revision has been comitted. They are not protected via
-- transactions.
setRevisionProp :: FileSystem   -- ^ The filesystem
                -> RevNum       -- ^ The revision
                -> String       -- ^ The property name
                -> Maybe String -- ^ The property value
                -> IO ()
setRevisionProp fs revNum name valStr
    = do pool <- newPool
         let value = fmap B8.pack valStr
         withFSPtr fs $ \ fsPtr ->
             withCString    name  $ \ namePtr  ->
             withSvnString' value $ \ valuePtr ->
             withPoolPtr    pool  $ \ poolPtr  ->
             svnErr $ _change_rev_prop fsPtr (fromIntegral revNum) namePtr valuePtr poolPtr

-- |@'getNodeHistory'@ /lazily/ reads the change history of given node
-- in a filesystem. The most recent change comes first in the
-- resulting list.
--
-- Revisions in the resulting list will be older than or the same age
-- as the revision of that node in the target revision of 'Rev'
-- monad. That is, if the 'Rev' monad is running on revision @X@, and
-- the node has been modified in some revisions younger than @X@,
-- those revisions younger than @X@ will not be included in the list.
getNodeHistory
    :: Bool                     -- ^ If 'True', stepping backwards in
                                --   history would cross a copy
                                --   operation. This is usually the
                                --   desired behavior.
    -> FilePath                 -- ^ The path to node you want to read
                                --   history.
    -> Rev [(RevNum, FilePath)] -- ^ A list of @(revNum, nodePath)@:
                                --   the node was modified somehow at
                                --   revision @revNum@, and at that
                                --   time the node was located on
                                --   @nodePath@.
getNodeHistory crossCopies path
    = do pool <- unsafeIOToFS newPool
         root <- getRoot
         unsafeIOToFS $ alloca $ \ histPtrPtr ->
             withFSRootPtr root $ \ rootPtr ->
             withCString   path $ \ pathPtr ->
             withPoolPtr   pool $ \ poolPtr ->
             do svnErr $ _node_history histPtrPtr rootPtr pathPtr poolPtr
                lazyReadHist pool =<< peek histPtrPtr
    where
      lazyReadHist pool histPtr = unsafeInterleaveIO $ readHist pool histPtr

      readHist :: Pool -> Ptr SVN_FS_HISTORY_T -> IO [(RevNum, FilePath)]
      readHist pool histPtr
          = alloca           $ \ histPtrPtr ->
            withPoolPtr pool $ \ poolPtr    ->
            do svnErr $ _history_prev histPtrPtr histPtr (marshalBool crossCopies) poolPtr
               got <- peek histPtrPtr

               if got == nullPtr then
                   -- We reached at the end of history. The pool may
                   -- be freed from now.
                   touchPool pool >> return []
                 else
                   do x  <- getHistLocation got pool
                      xs <- lazyReadHist pool got
                      return (x:xs)

      getHistLocation :: Ptr SVN_FS_HISTORY_T -> Pool -> IO (RevNum, FilePath)
      getHistLocation histPtr pool
          = alloca           $ \ pathPtrPtr ->
            alloca           $ \ revNumPtr  ->
            withPoolPtr pool $ \ poolPtr    ->
            do svnErr $ _history_location pathPtrPtr revNumPtr histPtr poolPtr
               revNum <- return . fromIntegral =<< peek revNumPtr
               path'  <- peekCString           =<< peek pathPtrPtr
               return (revNum, path')
