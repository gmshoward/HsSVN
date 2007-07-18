{- -*- haskell -*- -}
module Subversion.Repository
    ( Repository

    , openRepository
    )
    where

import           Foreign          hiding (Pool, newPool)
import           Foreign.C
import qualified GHC.ForeignPtr   as GF
import           Subversion.Error
import           Subversion.Pool


newtype Repository = Repository (ForeignPtr SVN_REPOS_T)
data SVN_REPOS_T


foreign import ccall "svn_repos_open"
        _open :: Ptr (Ptr SVN_REPOS_T) -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)


wrapRepos :: Pool -> Ptr SVN_REPOS_T -> IO Repository
wrapRepos pool reposPtr
    = do repos <- newForeignPtr_ reposPtr
         GF.addForeignPtrConcFinalizer repos $ touchPool pool
         return $ Repository repos


openRepository :: FilePath -> IO Repository
openRepository path
    = do pool <- newPool
         alloca $ \ reposPtrPtr ->
             withCString path $ \ pathPtr ->
                 withPoolPtr pool $ \ poolPtr ->
                     do svnErr $ _open reposPtrPtr pathPtr poolPtr
                        wrapRepos pool =<< peek reposPtrPtr
