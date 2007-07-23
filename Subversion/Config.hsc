{- -*- haskell -*- -}
module Subversion.Config
    ( Config
    )
    where

import           GHC.ForeignPtr  as GF
import           Subversion.Hash

newtype Config = Config (ForeignPtr SVN_CONFIG_T)
data SVN_CONFIG_T


instance HashValue Config where
    marshal (Config config)
        = return $ castForeignPtr config

    unmarshal finalizer configPtr
        = do config <- newForeignPtr_ configPtr
             GF.addForeignPtrConcFinalizer config finalizer
             return $ Config $ castForeignPtr config
