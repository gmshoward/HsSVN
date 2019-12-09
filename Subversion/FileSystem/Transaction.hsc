{-# LANGUAGE
    DeriveDataTypeable
  , EmptyDataDecls
  , ForeignFunctionInterface
  #-}
{-# OPTIONS_HADDOCK prune #-}

-- |An interface to functions that work on a filesystem transaction.

module Subversion.FileSystem.Transaction
    ( -- * Type 
      Txn

    , Transaction -- private
    , SVN_FS_TXN_T -- private

    , wrapTxn -- private
    , withTxnPtr -- private
    , runTxn -- private

    , abortTxn -- private
    , getTransactionRoot -- private

      -- * Accessing transaction property
    , getTxnProp
    , getTxnPropList
    , setTxnProp

      -- * Changing content of file
    , applyText
    , applyTextLBS

      -- * Changing node property
    , setNodeProp

      -- * Creating, deleting and copying entry
    , makeFile
    , makeDirectory

    , deleteEntry

    , copyEntry
    )
    where

import           Control.Monad.Reader
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as Lazy        (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8   hiding (ByteString)
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.Error
import           Subversion.FileSystem.Revision
import           Subversion.FileSystem.Root
import           Subversion.Hash
import           Subversion.Pool
import           Subversion.Stream
import           Subversion.String
import           Subversion.Types

{- Monad Txn ----------------------------------------------------------------- -}

data TxnContext = TxnContext {
      ctxTxn  :: Transaction
    , ctxRoot :: FileSystemRoot
    }

-- |@'Txn' a@ is a FS monad which reads and updates data in filesystem
-- and finally returns @a@. See 'Subversion.FileSystem.Root.MonadFS'.
newtype Txn a = Txn { unTxn :: ReaderT TxnContext IO a }

instance Functor Txn where
    fmap f c = Txn (fmap f (unTxn c))

instance Monad Txn where
    c >>= f = Txn (unTxn c >>= unTxn . f)
    return  = Txn . return
    fail    = Txn . fail

instance Applicative Txn where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance MonadFS Txn where
    getRoot        = Txn (fmap ctxRoot ask)
    unsafeIOToFS a = Txn (liftIO a)
    isTransaction  = Txn (return True)


getTxn :: Txn Transaction
getTxn = Txn (fmap ctxTxn ask)


{- functions and types ------------------------------------------------------- -}

newtype Transaction = Transaction (ForeignPtr SVN_FS_TXN_T)
data SVN_FS_TXN_T


foreign import ccall "svn_fs_abort_txn"
        _abort_txn :: Ptr SVN_FS_TXN_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall "svn_fs_txn_root"
        _txn_root :: Ptr (Ptr SVN_FS_ROOT_T) -> Ptr SVN_FS_TXN_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_txn_prop"
        _txn_prop :: Ptr (Ptr SVN_STRING_T) -> Ptr SVN_FS_TXN_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_txn_proplist"
        _txn_proplist :: Ptr (Ptr APR_HASH_T) -> Ptr SVN_FS_TXN_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_change_txn_prop"
        _change_txn_prop :: Ptr SVN_FS_TXN_T -> CString -> Ptr SVN_STRING_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_change_node_prop"
        _change_node_prop :: Ptr SVN_FS_ROOT_T -> CString -> CString -> Ptr SVN_STRING_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_apply_text"
        _apply_text :: Ptr (Ptr SVN_STREAM_T) -> Ptr SVN_FS_ROOT_T -> CString -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_make_file"
        _make_file :: Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_make_dir"
        _make_dir :: Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_delete"
        _delete :: Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_copy"
        _copy :: Ptr SVN_FS_ROOT_T -> CString -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapTxn :: IO () -> Ptr SVN_FS_TXN_T -> IO Transaction
wrapTxn finalizer txnPtr
    = do txn <- newForeignPtr_ txnPtr
         GF.addForeignPtrConcFinalizer txn finalizer
         return $ Transaction txn


touchTxn :: Transaction -> IO ()
touchTxn (Transaction txn) = touchForeignPtr txn


withTxnPtr :: Transaction -> (Ptr SVN_FS_TXN_T -> IO a) -> IO a
withTxnPtr (Transaction txn) = withForeignPtr txn


runTxn :: Txn a -> Transaction -> IO a
runTxn c txn
    = do root <- getTransactionRoot txn

         -- We've got the txn root so we can run the Txn monad.
         let ctx = TxnContext {
                     ctxTxn  = txn
                   , ctxRoot = root
                   }

         runReaderT (unTxn c) ctx


abortTxn :: Transaction -> IO ()
abortTxn txn
    = do pool <- newPool
         withTxnPtr txn $ \ txnPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 svnErr $ _abort_txn txnPtr poolPtr


getTransactionRoot :: Transaction -> IO FileSystemRoot
getTransactionRoot txn
    = do pool <- newPool
         alloca $ \ rootPtrPtr ->
             withTxnPtr  txn  $ \ txnPtr  ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr (_txn_root rootPtrPtr txnPtr poolPtr)
             >>  peek rootPtrPtr
             >>= wrapFSRoot
                     -- The root depends on both pool and txn.
                     (touchPool pool >> touchTxn txn)

-- |@'getTxnProp' propName@ returns the value of the property named
-- @propName@ on the transaction.
getTxnProp :: String -> Txn (Maybe String)
getTxnProp name
    = do txn  <- getTxn
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ valPtrPtr ->
             withTxnPtr  txn  $ \ txnPtr  ->
             withCString name $ \ namePtr ->
             withPoolPtr pool $ \ poolPtr ->
                 do svnErr $ _txn_prop valPtrPtr txnPtr namePtr poolPtr
                    prop <- peekSvnString' =<< peek valPtrPtr
                    -- We read prop in the pool so we don't want pool
                    -- to be freed that time.
                    touchPool pool
                    return $ fmap B8.unpack prop

-- |@'getTxnPropList'@ returns the entire property list on the
-- transaction.
getTxnPropList :: Txn [(String, String)]
getTxnPropList 
    = do txn  <- getTxn
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ hashPtrPtr ->
             withTxnPtr  txn  $ \ txnPtr  ->
             withPoolPtr pool $ \ poolPtr ->
                 do svnErr $ _txn_proplist hashPtrPtr txnPtr poolPtr
                    hash <- wrapHash (touchPool pool) =<< peek hashPtrPtr
                    mapHash' (\ (n, v)
                                  -> fmap ((,) n . B8.unpack) (peekSvnString v))
                             hash

-- |@'setTxnProp' propName propValue@ changes, adds or deletes a
-- property on the transaction.
setTxnProp :: String -> Maybe String -> Txn ()
setTxnProp name valStr
    = do txn  <- getTxn
         pool <- unsafeIOToFS newPool
         let value = fmap B8.pack valStr
         unsafeIOToFS $ withTxnPtr txn $ \ txnPtr ->
             withCString    name  $ \ namePtr  ->
             withSvnString' value $ \ valuePtr ->
             withPoolPtr    pool  $ \ poolPtr  ->
             svnErr $ _change_txn_prop txnPtr namePtr valuePtr poolPtr

-- |@'setNodeProp' fpath propName propValue@ changes, adds or deletes
-- a property named @propName@ on file @fpath@.
setNodeProp :: FilePath -> String -> Maybe String -> Txn ()
setNodeProp path name valStr
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         let value = fmap B8.pack valStr
         unsafeIOToFS $ withFSRootPtr root $ \ rootPtr ->
             withCString    path  $ \ pathPtr  ->
             withCString    name  $ \ namePtr  ->
             withSvnString' value $ \ valuePtr ->
             withPoolPtr    pool  $ \ poolPtr  ->
             svnErr $ _change_node_prop rootPtr pathPtr namePtr valuePtr poolPtr


-- |@'applyText'@ replaces the content of file.
applyText
    :: FilePath     -- ^ The file to be replaced. If it does not exist
                    --   in the transaction, 'applyText' throws an
                    --   error. That is, you can't use this action to
                    --   create a new file; use 'makeFile' to create
                    --   an empty file first.
    -> Maybe String -- ^ The hex MD5 digest for the new content. It is
                    --   ignored if 'Prelude.Nothing'. But if it's not
                    --   'Prelude.Nothing', it must match the checksum
                    --   of the content or 'applyText' throws an
                    --   error.
    -> String       -- ^ The new content. 
                    --
                    --   This argument is currently a 'Prelude.String'
                    --   but someday it may be changed to
                    --   @['Data.Word.Word8']@ or something alike. See
                    --   'Subversion.FileSystem.Root.getFileContents'.
    -> Txn ()
applyText path resultMD5 contents
    = applyTextLBS path resultMD5 (L8.pack contents)

-- |@'applyTextLBS'@ does the same thing as 'applyText' but takes
-- 'Data.ByteString.Lazy.ByteString' instead.
applyTextLBS :: FilePath -> Maybe String -> Lazy.ByteString -> Txn ()
applyTextLBS path resultMD5 contents
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ alloca $ \ ioPtrPtr ->
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

-- |@'makeFile' fpath@ creates a new empty file named @fpath@. The
-- file is initially empty and has no properties.
makeFile :: FilePath -> Txn ()
makeFile path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ withFSRootPtr root $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _make_file rootPtr pathPtr poolPtr

-- |@'makeDirectory' fpath@ creates a new directory named @fpath@. The
-- new directory has no entries, and no properties.
makeDirectory :: FilePath -> Txn ()
makeDirectory path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ withFSRootPtr root $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _make_dir rootPtr pathPtr poolPtr

-- |@'deleteEntry' fpath@ delete the node named @fpath@ in the
-- transaction. If the node being deleted is a directory, its contents
-- will be deleted recursively.
--
-- If the @fpath@ is missing from the transaction, 'deleteEntry'
-- throws an error.
--
-- Attempting to remove the root directory also results in an error,
-- even if the directory is empty.
deleteEntry :: FilePath -> Txn ()
deleteEntry path
    = do root <- getRoot
         pool <- unsafeIOToFS newPool
         unsafeIOToFS $ withFSRootPtr root $ \ rootPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
             svnErr $ _delete rootPtr pathPtr poolPtr

-- |@'copyEntry' fromRevNum fromPath toPath@ creates a copy of the
-- file @fromPath@ in a revision @fromRevNum@ to @toPath@ in the
-- transaction.
copyEntry :: RevNum -> FilePath -> FilePath -> Txn ()
copyEntry fromRevNum fromPath toPath
    = do toRoot <- getRoot
         fs     <- unsafeIOToFS $ getRootFS toRoot
         unsafeIOToFS $ withRevision fs fromRevNum
             $ do fromRoot <- getRoot
                  pool     <- unsafeIOToFS newPool
                  unsafeIOToFS $ withFSRootPtr fromRoot $ \ fromRootPtr ->
                      withCString   fromPath $ \ fromPathPtr ->
                      withFSRootPtr toRoot   $ \ toRootPtr   ->
                      withCString   toPath   $ \ toPathPtr   ->
                      withPoolPtr   pool     $ \ poolPtr     ->
                      svnErr $ _copy fromRootPtr fromPathPtr toRootPtr toPathPtr poolPtr
