{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.FileSystem
    ( FileSystem
    , SVN_FS_T

    , wrapFS
    , withFSPtr
    , touchFS

    , getYoungestRev
    )
    where

import           Control.Monad
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.Error
import           Subversion.Pool
import           Subversion.Types


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
