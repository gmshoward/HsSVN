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

    unmarshal hash configPtr
        = do config <- newForeignPtr_ configPtr
             -- configPtr よりも hash が先に消えては困る。
             GF.addForeignPtrConcFinalizer config $ touchForeignPtr hash
             return $ Config $ castForeignPtr config
