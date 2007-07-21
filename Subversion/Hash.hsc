{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.Hash
    ( Hash
    , HashValue(..)
    , APR_HASH_T

    , withHashPtr
    , unsafeHashToPtr
    , unsafeHashToPtr'
    , touchHash
    , touchHash'

    , new
    , update
    , delete
    , lookup
    )
    where


import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Utils hiding (new)
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.ForeignPtr        as GF
import           Prelude               hiding (lookup)
import           Subversion.Pool
import           Subversion.Types


newtype Hash a = Hash (ForeignPtr APR_HASH_T)
data APR_HASH_T


mallocStringForeignPtr :: String -> IO (ForeignPtr CChar)
mallocStringForeignPtr str
    = withCStringLen str $ \ (strPtr, len) ->
      do strFPtr <- mallocForeignPtrBytes $ len + 1
         copyBytes (unsafeForeignPtrToPtr strFPtr) strPtr len
         pokeByteOff (unsafeForeignPtrToPtr strFPtr) len '\0'
         return strFPtr


class HashValue a where
    marshal   :: a -> IO (ForeignPtr ())
    unmarshal :: ForeignPtr APR_HASH_T -> Ptr () -> IO a

instance HashValue String where
    marshal str
        = mallocStringForeignPtr str >>= return . castForeignPtr

    unmarshal hash strPtr
        = do str <- peekCString (castPtr strPtr)
             -- strPtr は hash の解放と同時に解放され得るのだが、
             -- peekCString する前にそれが起きては困る。
             touchForeignPtr hash
             return str


foreign import ccall unsafe "apr_hash_make"
        _make :: Ptr APR_POOL_T -> IO (Ptr APR_HASH_T)

foreign import ccall unsafe "apr_hash_set"
        _set :: Ptr APR_HASH_T -> Ptr () -> APR_SSIZE_T -> Ptr () -> IO ()

foreign import ccall unsafe "apr_hash_get"
        _get :: Ptr APR_HASH_T -> Ptr () -> APR_SSIZE_T -> IO (Ptr ())


wrapHash :: HashValue a => Pool -> Ptr APR_HASH_T -> IO (Hash a)
wrapHash pool hashPtr
    = do hash <- newForeignPtr_ hashPtr
         GF.addForeignPtrConcFinalizer hash $ touchPool pool
         return $ Hash hash


withHashPtr :: Hash a -> (Ptr APR_HASH_T -> IO b) -> IO b
withHashPtr (Hash hash) = withForeignPtr hash


unsafeHashToPtr :: Hash a -> Ptr APR_HASH_T
unsafeHashToPtr (Hash hash) = unsafeForeignPtrToPtr hash


unsafeHashToPtr' :: Maybe (Hash a) -> Ptr APR_HASH_T
unsafeHashToPtr' Nothing     = nullPtr
unsafeHashToPtr' (Just hash) = unsafeHashToPtr hash


touchHash :: Hash a -> IO ()
touchHash (Hash hash) = touchForeignPtr hash


touchHash' :: Maybe (Hash a) -> IO ()
touchHash' Nothing     = return ()
touchHash' (Just hash) = touchHash hash


new :: HashValue a => IO (Hash a)
new = do pool <- newPool
         withPoolPtr pool $ \ poolPtr ->
             _make poolPtr >>= wrapHash pool

-- 一旦 Hash に入れた値は、Hash 自体が解放されるまでは解放されなくなる
-- 事に注意。それがまずいのであれば、Hash 自体に Map (Ptr ())
-- (ForeignPtr ()) を持たせて、その Map を同時に管理しなければならない。
-- 面倒だ。
update :: HashValue a => Hash a -> String -> a -> IO ()
update (Hash hash) key value
    = withForeignPtr hash $ \ hashPtr ->
      do keyFPtr   <- mallocStringForeignPtr key
         valueFPtr <- marshal value
         _set hashPtr
              (castPtr $ unsafeForeignPtrToPtr keyFPtr)
              (#const APR_HASH_KEY_STRING)
              (unsafeForeignPtrToPtr valueFPtr)
         -- hash よりも key 及び value が先に解放されては困る。
         GF.addForeignPtrConcFinalizer hash
               $ do touchForeignPtr keyFPtr
                    touchForeignPtr valueFPtr


delete :: HashValue a => Hash a -> String -> IO ()
delete hash key
    = withHashPtr    hash $ \ hashPtr ->
      withCStringLen key  $ \ (keyPtr, keyLen) ->
      _set hashPtr (castPtr keyPtr) (fromIntegral keyLen) nullPtr


lookup :: HashValue a => Hash a -> String -> IO (Maybe a)
lookup (Hash hash) key
    = withForeignPtr hash $ \ hashPtr ->
      withCStringLen key  $ \ (keyPtr, keyLen) ->
      do valuePtr <- _get hashPtr (castPtr keyPtr) (fromIntegral keyLen)
         if valuePtr == nullPtr then
             return Nothing
           else
             do value <- unmarshal hash valuePtr
                -- valuePtr は hash の解放と同時に解放され得るのだが、
                -- unmarshal する前にそれが起きては困る。
                touchForeignPtr hash
                return $ Just value