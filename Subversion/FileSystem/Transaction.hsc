{- -*- haskell -*- -}

module Subversion.FileSystem.Transaction
    ( Transaction
    , SVN_FS_TXN_T

    , wrapTxn
    , withTxnPtr

    , abortTxn
    , getTransactionRoot
    )
    where

import           Control.Monad
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.FileSystem.Root
import           Subversion.Error
import           Subversion.Pool


newtype Transaction = Transaction (ForeignPtr SVN_FS_TXN_T)
data SVN_FS_TXN_T


foreign import ccall "svn_fs_abort_txn"
        _abort_txn :: Ptr SVN_FS_TXN_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall "svn_fs_txn_root"
        _txn_root :: Ptr (Ptr SVN_FS_ROOT_T) -> Ptr SVN_FS_TXN_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapTxn :: IO () -> Ptr SVN_FS_TXN_T -> IO Transaction
wrapTxn finalizer txnPtr
    = do txn <- newForeignPtr_ txnPtr
         GF.addForeignPtrConcFinalizer txn finalizer
         return $ Transaction txn


touchTxn :: Transaction -> IO ()
touchTxn (Transaction txn) = touchForeignPtr txn


withTxnPtr :: Transaction -> (Ptr SVN_FS_TXN_T -> IO a) -> IO a
withTxnPtr (Transaction txn) = withForeignPtr txn


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
             (svnErr $ _txn_root rootPtrPtr txnPtr poolPtr)
             >>  peek rootPtrPtr
             >>= (wrapFSRoot $
                  -- root は pool にも txn にも依存する。
                  touchPool pool >> touchTxn txn)
