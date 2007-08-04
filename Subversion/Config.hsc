{- -*- haskell -*- -}

-- |An interface to client configuration files (svn_config.h).
--
-- As you see, this module is totally incomplete. If you need this,
-- you must write a patch.

module Subversion.Config
    ( Config
    )
    where

import           GHC.ForeignPtr  as GF
import           Subversion.Hash

-- |@'Config'@ represents an opaque structure describing a set of
-- configuration options. There is currently no way to neither create
-- nor inspect this object.
newtype Config = Config (ForeignPtr SVN_CONFIG_T)
data SVN_CONFIG_T


instance HashValue Config where
    marshal (Config config)
        = return $ castForeignPtr config

    unmarshal finalizer configPtr
        = do config <- newForeignPtr_ configPtr
             GF.addForeignPtrConcFinalizer config finalizer
             return $ Config $ castForeignPtr config
