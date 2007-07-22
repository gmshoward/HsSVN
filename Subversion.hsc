{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion
    ( withSubversion
    )
    where

import           Control.Exception
import           Subversion.Error


foreign import ccall "HsSVN_initialize"
        _initialize :: IO Int


withSubversion :: IO a -> IO a
withSubversion f
    = do ret <- _initialize
         case ret of
           0 -> catchDyn f rethrowSvnError
           _ -> fail "Subversion: failed to initialize APR."


rethrowSvnError :: SvnError -> IO a
rethrowSvnError err
    = let code = svnErrCode err
          msg  = svnErrMsg  err
      in
        fail $ "withSubversion: caught an SvnError: " ++ (show code) ++ ": " ++ msg
