{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.FileSystem
    ( FileSystem
    , SVN_FS_T

    , wrapFS

    , getYoungestRev

    , FileSystemRoot
    , getRevisionRoot

    , getFileContents
    , getFileContentsLBS

    , applyText
    , applyTextLBS

    , Transaction
    , SVN_FS_TXN_T

    , wrapTxn
    , withTxnPtr

    , abortTxn
    , getTransactionRoot
    )
    where

import           Control.Monad
import           Data.ByteString.Base
import qualified Data.ByteString.Lazy.Char8 as L8
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.Error
import           Subversion.Pool
import           Subversion.Stream
import           Subversion.Types


{- filesystem ---------------------------------------------------------------- -}

newtype FileSystem = FileSystem (ForeignPtr SVN_FS_T)
data SVN_FS_T

foreign import ccall "svn_fs_youngest_rev"
        _youngest_rev :: Ptr SVN_REVNUM_T -> Ptr SVN_FS_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapFS :: ForeignPtr a -> Ptr SVN_FS_T -> IO FileSystem
wrapFS dependent fsPtr
    = do fs <- newForeignPtr_ fsPtr
         GF.addForeignPtrConcFinalizer fs $ touchForeignPtr dependent
         return $ FileSystem fs


withFSPtr :: FileSystem -> (Ptr SVN_FS_T -> IO a) -> IO a
withFSPtr (FileSystem fs) = withForeignPtr fs


touchFS :: FileSystem -> IO ()
touchFS (FileSystem fs) = touchForeignPtr fs


getYoungestRev :: FileSystem -> IO Int
getYoungestRev fs
    = do pool <- newPool
         alloca $ \ revNumPtr ->
             withFSPtr fs $ \ fsPtr ->
                 withPoolPtr pool $ \ poolPtr ->
                     (svnErr $ _youngest_rev revNumPtr fsPtr poolPtr)
                     >>  peek revNumPtr
                     >>= return . fromIntegral


{- filesystem roots ---------------------------------------------------------- -}

newtype FileSystemRoot = FileSystemRoot (ForeignPtr SVN_FS_ROOT_T)
data SVN_FS_ROOT_T


foreign import ccall unsafe "svn_fs_revision_root"
        _revision_root :: Ptr (Ptr SVN_FS_ROOT_T) -> Ptr SVN_FS_T -> SVN_REVNUM_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_file_contents"
        _file_contents :: Ptr (Ptr SVN_STREAM_T) -> Ptr SVN_FS_ROOT_T -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_fs_apply_text"
        _apply_text :: Ptr (Ptr SVN_STREAM_T) -> Ptr SVN_FS_ROOT_T -> CString -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapFSRoot :: IO () -> Ptr SVN_FS_ROOT_T -> IO FileSystemRoot
wrapFSRoot finalizer rootPtr
    = do root <- newForeignPtr_ rootPtr
         GF.addForeignPtrConcFinalizer root finalizer
         return $ FileSystemRoot root


withFSRootPtr :: FileSystemRoot -> (Ptr SVN_FS_ROOT_T -> IO a) -> IO a
withFSRootPtr (FileSystemRoot root) = withForeignPtr root


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


getFileContents :: FileSystemRoot -> String -> IO String
getFileContents root path
    = liftM L8.unpack $ getFileContentsLBS root path


getFileContentsLBS :: FileSystemRoot -> String -> IO LazyByteString
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


applyText :: FileSystemRoot -> String -> Maybe String -> String -> IO ()
applyText root path resultMD5 contents
    = applyTextLBS root path resultMD5 (L8.pack contents)


applyTextLBS :: FileSystemRoot -> String -> Maybe String -> LazyByteString -> IO ()
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


{- transaction --------------------------------------------------------------- -}

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
