{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.Types
    ( APR_SIZE_T
    , APR_SSIZE_T
    , APR_STATUS_T

    , SVN_BOOLEAN_T
    , SVN_NODE_KIND_T
    , SVN_REVNUM_T
    , SVN_VERSION_T

    , marshalBool
    , unmarshalBool

    , NodeKind(..)
    , unmarshalNodeKind

    , Version
    , peekVersion
    )
    where

import           Data.Int
import           Data.Word
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Storable


type APR_SIZE_T    = #type apr_size_t
type APR_SSIZE_T   = #type apr_ssize_t
type APR_STATUS_T  = #type apr_status_t

type SVN_BOOLEAN_T   = #type svn_boolean_t
type SVN_NODE_KIND_T = #type svn_node_kind_t
type SVN_REVNUM_T    = #type svn_revnum_t
data SVN_VERSION_T


marshalBool :: Bool -> SVN_BOOLEAN_T
marshalBool True  = (#const TRUE )
marshalBool False = (#const FALSE)

unmarshalBool :: SVN_BOOLEAN_T -> Bool
unmarshalBool (#const TRUE ) = True
unmarshalBool (#const FALSE) = False


data NodeKind
    = NoNode
    | FileNode
    | DirNode
      deriving (Show, Eq)

unmarshalNodeKind :: SVN_NODE_KIND_T -> NodeKind
unmarshalNodeKind (#const svn_node_none) = NoNode
unmarshalNodeKind (#const svn_node_file) = FileNode
unmarshalNodeKind (#const svn_node_dir ) = DirNode


data Version = Version {
      verMajor :: Int
    , verMinor :: Int
    , verPatch :: Int
    , verTag   :: String
    } deriving (Show, Eq)


peekVersion :: Ptr SVN_VERSION_T -> IO Version
peekVersion obj
    = do major <- peekVerMajor obj
         minor <- peekVerMinor obj
         patch <- peekVerPatch obj
         tag   <- peekVerTag   obj >>= peekCString
         return Version {
                      verMajor = major
                    , verMinor = minor
                    , verPatch = patch
                    , verTag   = tag
                    }


peekVerMajor :: Ptr SVN_VERSION_T -> IO Int
peekVerMajor = (#peek svn_version_t, major)

peekVerMinor :: Ptr SVN_VERSION_T -> IO Int
peekVerMinor = (#peek svn_version_t, minor)

peekVerPatch :: Ptr SVN_VERSION_T -> IO Int
peekVerPatch = (#peek svn_version_t, patch)

peekVerTag :: Ptr SVN_VERSION_T -> IO CString
peekVerTag = (#peek svn_version_t, tag)