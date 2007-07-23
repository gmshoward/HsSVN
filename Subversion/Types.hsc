{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.Types
    ( APR_SIZE_T
    , APR_SSIZE_T
    , APR_STATUS_T
    , SVN_REVNUM_T

    , SVN_NODE_KIND_T
    , NodeKind(..)
    , unmarshalNodeKind
    )
    where

import           Data.Int
import           Data.Word

type APR_SIZE_T   = #type apr_size_t
type APR_SSIZE_T  = #type apr_ssize_t
type APR_STATUS_T = #type apr_status_t
type SVN_REVNUM_T = #type svn_revnum_t


type SVN_NODE_KIND_T = #type svn_node_kind_t

data NodeKind
    = FileNode
    | DirNode
      deriving (Show, Eq)

unmarshalNodeKind :: SVN_NODE_KIND_T -> NodeKind
unmarshalNodeKind (#const svn_node_file) = FileNode
unmarshalNodeKind (#const svn_node_dir ) = DirNode
