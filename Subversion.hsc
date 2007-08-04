{- -*- haskell -*- -}

-- |HsSVN is a (part of) Subversion binding for Haskell. Currently it
-- can do most things related to the Subversion FS but others are left
-- uncoverd.
--
-- If you find out any features you want aren't supported, you must
-- write your own patch (or take over the HsSVN project). Happy
-- hacking.

#include "HsSVN.h"

module Subversion
    ( withSubversion
    )
    where

import           Control.Exception
import           Subversion.Error


foreign import ccall "HsSVN_initialize"
        _initialize :: IO Int


-- |Computation of @'withSubversion' action@ initializes the
-- Subversion library and computes @action@. Every applications that
-- use HsSVN must wrap any operations related to Subversion with
-- 'withSubversion', or they will crash.
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
