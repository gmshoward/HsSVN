{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.FileSystem
    ( FileSystem
    , SVN_FS_T

    , wrapFS

    , FileSystemRoot
    , getRevisionRoot
    )
    where

import           Data.Int
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.Error
import           Subversion.Pool


{- filesystem ---------------------------------------------------------------- -}

newtype FileSystem = FileSystem (ForeignPtr SVN_FS_T)
data SVN_FS_T

type SVN_REVNUM_T = #type svn_revnum_t


wrapFS :: ForeignPtr a -> Ptr SVN_FS_T -> IO FileSystem
wrapFS dependent fsPtr
    = do fs <- newForeignPtr_ fsPtr
         GF.addForeignPtrConcFinalizer fs $ touchForeignPtr dependent
         return $ FileSystem fs


withFSPtr :: FileSystem -> (Ptr SVN_FS_T -> IO a) -> IO a
withFSPtr (FileSystem fs) = withForeignPtr fs


touchFS :: FileSystem -> IO ()
touchFS (FileSystem fs) = touchForeignPtr fs


{- filesystem roots ---------------------------------------------------------- -}

newtype FileSystemRoot = FileSystemRoot (ForeignPtr SVN_FS_ROOT_T)
data SVN_FS_ROOT_T


foreign import ccall unsafe "svn_fs_revision_root"
        _revision_root :: Ptr (Ptr SVN_FS_ROOT_T) -> Ptr SVN_FS_T -> SVN_REVNUM_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapFSRoot :: FileSystem -> Pool -> Ptr SVN_FS_ROOT_T -> IO FileSystemRoot
wrapFSRoot fs pool rootPtr
    -- rootPtr は fs にも pool にも依存してゐる。
    = do root <- newForeignPtr_ rootPtr
         GF.addForeignPtrConcFinalizer root
               $ do touchFS fs
                    touchPool pool
         return $ FileSystemRoot root


getRevisionRoot :: FileSystem -> Int -> IO FileSystemRoot
getRevisionRoot fs revNum
    = do pool <- newPool
         alloca $ \ rootPtrPtr ->
             withFSPtr fs $ \ fsPtr ->
                 withPoolPtr pool $ \ poolPtr ->
                     do svnErr $ _revision_root rootPtrPtr fsPtr (fromIntegral revNum) poolPtr
                        wrapFSRoot fs pool =<< peek rootPtrPtr