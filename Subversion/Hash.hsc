{- -*- haskell -*- -}
module Subversion.Hash
    ( Hash

    , new
    , update
    , delete
    , lookup
    )
    where

import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal.Utils hiding (new)
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.ForeignPtr        as GF
import           Prelude               hiding (lookup)
import           Subversion.Pool
import           System.Posix.Types


newtype Hash a = Hash (ForeignPtr APR_HASH_T)
data APR_HASH_T


class HashValue a where
    marshal   :: a -> IO (ForeignPtr ())
    unmarshal :: Ptr () -> IO a

instance HashValue String where
    marshal str
        = withCStringLen str $ \ (strPtr, len) ->
          do strFPtr <- mallocForeignPtrBytes $ len + 1
             copyBytes (unsafeForeignPtrToPtr strFPtr) strPtr len
             pokeByteOff (unsafeForeignPtrToPtr strFPtr) len '\0'
             return $ castForeignPtr strFPtr

    unmarshal strPtr
        = peekCString (castPtr strPtr)


foreign import ccall "apr_hash_make"
        _make :: Ptr APR_POOL_T -> IO (Ptr APR_HASH_T)

foreign import ccall "apr_hash_set"
        _set :: Ptr APR_HASH_T -> Ptr () -> CSsize -> Ptr () -> IO ()

foreign import ccall "apr_hash_get"
        _get :: Ptr APR_HASH_T -> Ptr () -> CSsize -> IO (Ptr ())


wrapHash :: HashValue a => Pool -> Ptr APR_HASH_T -> IO (Hash a)
wrapHash pool hashPtr
    = do hash <- newForeignPtr_ hashPtr
         GF.addForeignPtrConcFinalizer hash $ touchPool pool
         return $ Hash hash


withHashPtr :: Hash a -> (Ptr APR_HASH_T -> IO b) -> IO b
withHashPtr (Hash hash) = withForeignPtr hash


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
      withCStringLen key  $ \ (keyPtr, keyLen) ->
      do valueFPtr <- marshal value
         _set hashPtr
              (castPtr keyPtr)
              (fromIntegral keyLen)
              (unsafeForeignPtrToPtr valueFPtr)
         -- hash よりも value が先に解放されては困る。
         GF.addForeignPtrConcFinalizer hash $ touchForeignPtr valueFPtr


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
             do value <- unmarshal valuePtr
                -- valuePtr は hash の解放と同時に解放され得るのだが、
                -- unmarshal する前にそれが起きては困る。
                touchForeignPtr hash
                return $ Just value