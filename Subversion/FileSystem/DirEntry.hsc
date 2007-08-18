{- -*- haskell -*- -}

-- |This module only defines the 'DirEntry' type.

#include "HsSVN.h"

module Subversion.FileSystem.DirEntry
    ( DirEntry(..)
    )
    where

import           Foreign.C.String
import           Foreign.Storable
import           Subversion.Hash
import           Subversion.Types

-- |The type of a Subversion directory entry.
--
-- Note that @svn_fs_dirent_t.id@ is currently unavailable in this
-- binding. Add one if you really need it.
data DirEntry
    = DirEntry {
        entName :: String   -- ^ The name of this directory entry.
      , entKind :: NodeKind -- ^ The node kind.
      }
    deriving (Show, Eq)


instance HashValue DirEntry where
    marshal ent
        = fail "marshalling DirEntry is not supported"

    unmarshal finalizer entPtr
        = do namePtr <- (#peek svn_fs_dirent_t, name) entPtr
             name    <- peekCString namePtr
             kind    <- (#peek svn_fs_dirent_t, kind) entPtr
             finalizer
             return DirEntry {
                              entName = name
                            , entKind = unmarshalNodeKind kind
                            }
