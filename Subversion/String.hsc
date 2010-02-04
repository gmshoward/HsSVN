#include "HsSVN.h"

module Subversion.String
    ( SVN_STRING_T
    , withSvnString
    , withSvnString'
    , peekSvnString
    , peekSvnString'
    )
    where

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Char8 as B8 hiding (ByteString)
import           Data.ByteString.Unsafe
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Subversion.Types

data SVN_STRING_T


pokeData :: Ptr SVN_STRING_T -> Ptr CChar -> IO ()
pokeData = #poke svn_string_t, data

peekData :: Ptr SVN_STRING_T -> IO (Ptr CChar)
peekData = #peek svn_string_t, data

pokeLen :: Ptr SVN_STRING_T -> APR_SIZE_T -> IO ()
pokeLen = #poke svn_string_t, len

peekLen :: Ptr SVN_STRING_T -> IO APR_SIZE_T
peekLen = #peek svn_string_t, len


withSvnString :: Strict.ByteString -> (Ptr SVN_STRING_T -> IO a) -> IO a
withSvnString bs f
    = unsafeUseAsCStringLen bs         $ \ (bsBuf, bsLen) ->
      allocaBytes (#size svn_string_t) $ \ obj            ->
      do pokeData obj bsBuf
         pokeLen  obj (fromIntegral bsLen)
         f obj


withSvnString' :: Maybe Strict.ByteString -> (Ptr SVN_STRING_T -> IO a) -> IO a
withSvnString' Nothing   f = f nullPtr
withSvnString' (Just bs) f = withSvnString bs f


peekSvnString :: Ptr SVN_STRING_T -> IO Strict.ByteString
peekSvnString obj
    | obj == nullPtr
        = fail "peekSvnString: got a null pointer"
    | otherwise
        = do buf <- peekData obj
             len <- peekLen  obj
             B8.packCStringLen (buf, fromIntegral len)


peekSvnString' :: Ptr SVN_STRING_T -> IO (Maybe Strict.ByteString)
peekSvnString' obj
    | obj == nullPtr
        = return Nothing
    | otherwise
        = fmap Just (peekSvnString obj)
