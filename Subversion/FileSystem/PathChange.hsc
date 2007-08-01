{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.FileSystem.PathChange
    ( PathChange(..)
    , ChangeKind(..)
    )
    where

import           Data.Word
import           Foreign.Storable
import           Subversion.Hash
import           Subversion.Types


-- svn_fs_path_change_t.node_rev_id is currently unavailable in this
-- binding. Add one if you really need it.
data PathChange = PathChange {
      pcChangeKind :: ChangeKind
    , pcTextMod    :: Bool
    , pcPropMod    :: Bool
    } deriving (Show, Eq)


type SVN_FS_PATH_CHANGE_KIND_T = #type svn_fs_path_change_kind_t

data ChangeKind = ModifiedNode
                | AddedNode
                | DeletedNode
                | ReplacedNode
                  deriving (Show, Eq)


unmarshalChangeKind :: SVN_FS_PATH_CHANGE_KIND_T -> ChangeKind
unmarshalChangeKind (#const svn_fs_path_change_modify ) = ModifiedNode
unmarshalChangeKind (#const svn_fs_path_change_add    ) = AddedNode
unmarshalChangeKind (#const svn_fs_path_change_delete ) = DeletedNode
unmarshalChangeKind (#const svn_fs_path_change_replace) = ReplacedNode


instance HashValue PathChange where
    marshal pc
        = fail "marshalling PathChange is not supported"

    unmarshal finalizer pcPtr
        = do kind    <- (#peek svn_fs_path_change_t, change_kind) pcPtr
             textMod <- (#peek svn_fs_path_change_t, text_mod   ) pcPtr
             propMod <- (#peek svn_fs_path_change_t, prop_mod   ) pcPtr
             finalizer
             return PathChange {
                                pcChangeKind = unmarshalChangeKind kind
                              , pcTextMod    = unmarshalBool       textMod
                              , pcPropMod    = unmarshalBool       propMod
                              }
