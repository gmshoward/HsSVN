{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.Hash
    ( Hash
    , HashValue(..)
    , APR_HASH_T

    , withHashPtr
    , unsafeHashToPtr
    , touchHash
    , wrapHash

    , new
    , update
    , delete
    , lookup

    , pairsToHash
    , hashToPairs

    , hashToValues

    , mapHash
    , mapHash'
    )
    where

import           Control.Monad
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils hiding (new)
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.ForeignPtr        as GF
import           Prelude               hiding (lookup)
import           Subversion.Pool
import           Subversion.Types
import           System.IO.Unsafe


newtype Hash a = Hash (ForeignPtr APR_HASH_T)
data APR_HASH_T

newtype HashIndex a = HashIndex (ForeignPtr APR_HASH_INDEX_T)
data APR_HASH_INDEX_T


mallocStringForeignPtr :: String -> IO (ForeignPtr CChar)
mallocStringForeignPtr str
    = withCStringLen str $ \ (strPtr, len) ->
      do strFPtr <- mallocForeignPtrBytes $ len + 1
         copyBytes (unsafeForeignPtrToPtr strFPtr) strPtr len
         pokeByteOff (unsafeForeignPtrToPtr strFPtr) len '\0'
         return strFPtr


class HashValue a where
    marshal   :: a -> IO (ForeignPtr ())
    unmarshal :: IO () -> Ptr () -> IO a

instance HashValue String where
    marshal str
        = mallocStringForeignPtr str >>= return . castForeignPtr

    unmarshal finalizer strPtr
        = do str <- peekCString (castPtr strPtr)
             finalizer
             return str


foreign import ccall unsafe "apr_hash_make"
        _make :: Ptr APR_POOL_T -> IO (Ptr APR_HASH_T)

foreign import ccall unsafe "apr_hash_set"
        _set :: Ptr APR_HASH_T -> Ptr () -> APR_SSIZE_T -> Ptr () -> IO ()

foreign import ccall unsafe "apr_hash_get"
        _get :: Ptr APR_HASH_T -> Ptr () -> APR_SSIZE_T -> IO (Ptr ())

foreign import ccall unsafe "apr_hash_first"
        _first :: Ptr APR_POOL_T -> Ptr APR_HASH_T -> IO (Ptr APR_HASH_INDEX_T)

foreign import ccall unsafe "apr_hash_this"
        _this :: Ptr APR_HASH_INDEX_T -> Ptr (Ptr ()) -> Ptr APR_SSIZE_T -> Ptr (Ptr ()) -> IO ()

foreign import ccall unsafe "apr_hash_next"
        _next :: Ptr APR_HASH_INDEX_T -> IO (Ptr APR_HASH_INDEX_T)


wrapHash :: IO () -> Ptr APR_HASH_T -> IO (Hash a)
wrapHash finalizer hashPtr
    = do hash <- newForeignPtr_ hashPtr
         GF.addForeignPtrConcFinalizer hash finalizer
         return $ Hash hash


withHashPtr :: Hash a -> (Ptr APR_HASH_T -> IO b) -> IO b
withHashPtr (Hash hash) = withForeignPtr hash


unsafeHashToPtr :: Hash a -> Ptr APR_HASH_T
unsafeHashToPtr (Hash hash) = unsafeForeignPtrToPtr hash


touchHash :: Hash a -> IO ()
touchHash (Hash hash) = touchForeignPtr hash


wrapHashIdx :: IO () -> Ptr APR_HASH_INDEX_T -> IO (HashIndex a)
wrapHashIdx finalizer idxPtr
    = do idx <- newForeignPtr_ idxPtr
         GF.addForeignPtrConcFinalizer idx finalizer
         return $ HashIndex idx


withHashIdxPtr :: HashIndex a -> (Ptr APR_HASH_INDEX_T -> IO b) -> IO b
withHashIdxPtr (HashIndex idx) = withForeignPtr idx


touchHashIdx :: HashIndex a -> IO ()
touchHashIdx (HashIndex idx) = touchForeignPtr idx


new :: HashValue a => IO (Hash a)
new = do pool <- newPool
         withPoolPtr pool $ \ poolPtr ->
             _make poolPtr >>= wrapHash (touchPool pool)

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


delete :: Hash a -> String -> IO ()
delete hash key
    = withHashPtr    hash $ \ hashPtr ->
      withCStringLen key  $ \ (keyPtr, keyLen) ->
      _set hashPtr (castPtr keyPtr) (fromIntegral keyLen) nullPtr


lookup :: HashValue a => Hash a -> String -> IO (Maybe a)
lookup hash key
    = withHashPtr    hash $ \ hashPtr ->
      withCStringLen key  $ \ (keyPtr, keyLen) ->
      do valuePtr <- _get hashPtr (castPtr keyPtr) (fromIntegral keyLen)
         if valuePtr == nullPtr then
             return Nothing
           else
             -- valuePtr は hash の解放と同時に解放され得るのだが、
             -- unmarshal する前にそれが起きては困る。
             unmarshal (touchHash hash) valuePtr
             >>= return . Just


getFirst :: Hash a -> IO (Maybe (HashIndex a))
getFirst hash
    = do pool <- newPool
         withPoolPtr pool $ \ poolPtr ->
             withHashPtr hash $ \ hashPtr ->
             do idxPtr <- _first poolPtr hashPtr
                if idxPtr == nullPtr then
                    return Nothing
                  else
                    liftM Just $ wrapHashIdx (touchPool pool) idxPtr


getThis :: HashValue a => HashIndex a -> IO (String, a)
getThis idx
    = do (key, valPtr) <- getThis' idx
         val <- unmarshal (touchHashIdx idx) (castPtr valPtr)
         return (key, val)


getThis' :: HashIndex a -> IO (String, Ptr a)
getThis' idx
    = withHashIdxPtr idx $ \ idxPtr ->
      alloca $ \ keyPtrPtr ->
      alloca $ \ keyLenPtr ->
      alloca $ \ valPtrPtr ->
      do _this idxPtr keyPtrPtr keyLenPtr valPtrPtr
         keyPtr <- liftM castPtr      (peek keyPtrPtr)
         keyLen <- liftM fromIntegral (peek keyLenPtr)
         key    <- peekCStringLen (keyPtr, keyLen)
         valPtr <- peek valPtrPtr
         return (key, castPtr valPtr)


getNext :: HashIndex a -> IO (Maybe (HashIndex a))
getNext idx
    = withHashIdxPtr idx $ \ idxPtr ->
      do idxPtr' <- _next idxPtr
         if idxPtr' == nullPtr then
             return Nothing
           else
             liftM Just $ wrapHashIdx (touchHashIdx idx) idxPtr'
                    

pairsToHash :: HashValue a => [(String, a)] -> IO (Hash a)
pairsToHash xs
    = do hash <- new
         mapM_ (uncurry $ update hash) xs
         return hash


hashToPairs :: HashValue a => Hash a -> IO [(String, a)]
hashToPairs = mapHash (return . id)


hashToValues :: HashValue a => Hash a -> IO [a]
hashToValues = mapHash (return . snd)


mapHash :: HashValue a => ((String, a) -> IO b) -> Hash a -> IO [b]
mapHash f hash = getFirst hash >>= loop
    where
      loop Nothing    = return []
      loop (Just idx) = do x  <- f =<< getThis idx
                           xs <- unsafeInterleaveIO $
                                 (getNext idx >>= loop)
                           return (x:xs)


mapHash' :: ((String, Ptr a) -> IO b) -> Hash a -> IO [b]
mapHash' f hash = getFirst hash >>= loop
    where
      loop Nothing    = return []
      loop (Just idx) = do x  <- f =<< getThis' idx
                           xs <- unsafeInterleaveIO $
                                 (getNext idx >>= loop)
                           return (x:xs)