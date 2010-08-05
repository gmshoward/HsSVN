{- -*- haskell -*- -}

-- |This module only defines two types; 'PathChange' and 'ChangeKind'.

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

-- |@'PathChange'@ describes a change in a revision occured on a path.
-- 
-- Note that @svn_fs_path_change_t.node_rev_id@ is currently
-- unavailable in this binding. Add one if you really need it.
data PathChange = PathChange {
      pcChangeKind :: ChangeKind -- ^ Kind of change.
    , pcTextMod    :: Bool       -- ^ Was the content modified in any way?
    , pcPropMod    :: Bool       -- ^ Were there any properties modified?
    } deriving (Show, Eq)


type SVN_FS_PATH_CHANGE_KIND_T = #type svn_fs_path_change_kind_t

-- |The kind of change that occured on the path.
data ChangeKind = ModifiedPath -- ^ Only modified.
                | AddedPath    -- ^ The path was added in the txn.
                | DeletedPath  -- ^ The path was removed in the txn.
                | ReplacedPath -- ^ The path was removed and re-added in the txn.
                  deriving (Show, Eq)


unmarshalChangeKind :: SVN_FS_PATH_CHANGE_KIND_T -> ChangeKind
unmarshalChangeKind (#const svn_fs_path_change_modify ) = ModifiedPath
unmarshalChangeKind (#const svn_fs_path_change_add    ) = AddedPath
unmarshalChangeKind (#const svn_fs_path_change_delete ) = DeletedPath
unmarshalChangeKind (#const svn_fs_path_change_replace) = ReplacedPath
unmarshalChangeKind _                                   = undefined


instance HashValue PathChange where
    marshal _
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
