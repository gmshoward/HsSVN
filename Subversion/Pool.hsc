module Subversion.Pool
    ( Pool
    , APR_POOL_T
    , newPool
    , withPoolPtr
    , touchPool
    )
    where

import           Foreign hiding (Pool, newPool)


newtype Pool = Pool (ForeignPtr APR_POOL_T)
data APR_POOL_T


foreign import ccall unsafe "HsSVN_svn_pool_create"
        _create :: Ptr APR_POOL_T -> IO (Ptr APR_POOL_T)

foreign import ccall unsafe "&HsSVN_svn_pool_destroy"
        _destroy :: FunPtr (Ptr APR_POOL_T -> IO ())


newPool :: IO Pool
newPool = _create nullPtr
          >>= newForeignPtr _destroy
          >>= return . Pool


withPoolPtr :: Pool -> (Ptr APR_POOL_T -> IO a) -> IO a
withPoolPtr (Pool pool) = withForeignPtr pool


touchPool :: Pool -> IO ()
touchPool (Pool pool) = touchForeignPtr pool