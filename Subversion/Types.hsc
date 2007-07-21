{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.Types
    ( APR_SIZE_T
    , APR_SSIZE_T
    , APR_STATUS_T
    , SVN_REVNUM_T
    )
    where

import           Data.Int
import           Data.Word

type APR_SIZE_T   = #type apr_size_t
type APR_SSIZE_T  = #type apr_ssize_t
type APR_STATUS_T = #type apr_status_t
type SVN_REVNUM_T = #type svn_revnum_t
