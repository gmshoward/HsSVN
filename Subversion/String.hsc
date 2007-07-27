{- -*- Haskell -*- -}

#include "HsSVN.h"

module Subversion.String
    ( SVN_STRING_T
    , withSvnString
    , peekSvnString
    , peekSvnString'
    )
    where

import           Data.ByteString.Base
import qualified Data.ByteString.Char8 as B8
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Subversion.Types

data SVN_STRING_T


pokeData :: Ptr SVN_STRING_T -> Ptr CChar -> IO ()
pokeData = (#poke svn_string_t, data)

peekData :: Ptr SVN_STRING_T -> IO (Ptr CChar)
peekData = (#peek svn_string_t, data)

pokeLen :: Ptr SVN_STRING_T -> APR_SIZE_T -> IO ()
pokeLen = (#poke svn_string_t, len)

peekLen :: Ptr SVN_STRING_T -> IO APR_SIZE_T
peekLen = (#peek svn_string_t, len)


withSvnString :: ByteString -> (Ptr SVN_STRING_T -> IO a) -> IO a
withSvnString bs f
    = unsafeUseAsCStringLen bs         $ \ (bsBuf, bsLen) ->
      allocaBytes (#size svn_string_t) $ \ obj            ->
      do pokeData obj bsBuf
         pokeLen  obj (fromIntegral bsLen)
         f obj


peekSvnString :: Ptr SVN_STRING_T -> IO ByteString
peekSvnString obj
    | obj == nullPtr
        = fail "peekSvnString: got a null pointer"
    | otherwise
        = do buf <- peekData obj
             len <- peekLen  obj
             B8.copyCStringLen (buf, fromIntegral len)


peekSvnString' :: Ptr SVN_STRING_T -> IO (Maybe ByteString)
peekSvnString' obj
    | obj == nullPtr
        = return Nothing
    | otherwise
        = peekSvnString obj >>= return . Just
