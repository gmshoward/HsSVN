{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion
    ( withSubversion
    )
    where


foreign import ccall "HsSVN_initialize"
        _initialize :: IO Int


withSubversion :: IO a -> IO a
withSubversion f
    = do ret <- _initialize
         case ret of
           0 -> f
           _ -> fail "Subversion: failed to initialize APR."
