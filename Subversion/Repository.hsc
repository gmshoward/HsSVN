{- -*- haskell -*- -}
module Subversion.Repository
    ( Repository

    , openRepository
    , createRepository
    , deleteRepository

    , getRepositoryFS
    )
    where

import           Control.Monad
import           Data.Maybe
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.Config
import           Subversion.FileSystem
import           Subversion.Hash
import           Subversion.Error
import           Subversion.Pool


newtype Repository = Repository (ForeignPtr SVN_REPOS_T)
data SVN_REPOS_T


foreign import ccall unsafe "svn_repos_open"
        _open :: Ptr (Ptr SVN_REPOS_T) -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_repos_create"
        _create :: Ptr (Ptr SVN_REPOS_T) -> CString -> Ptr CChar -> Ptr CChar -> Ptr APR_HASH_T -> Ptr APR_HASH_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_repos_delete"
        _delete :: CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_repos_fs"
        _fs :: Ptr SVN_REPOS_T -> IO (Ptr SVN_FS_T)


wrapRepos :: Pool -> Ptr SVN_REPOS_T -> IO Repository
wrapRepos pool reposPtr
    = do repos <- newForeignPtr_ reposPtr
         GF.addForeignPtrConcFinalizer repos $ touchPool pool
         return $ Repository repos


withReposPtr :: Repository -> (Ptr SVN_REPOS_T -> IO a) -> IO a
withReposPtr (Repository repos) = withForeignPtr repos


openRepository :: FilePath -> IO Repository
openRepository path
    = do pool <- newPool
         alloca $ \ reposPtrPtr ->
             withCString path $ \ pathPtr ->
                 withPoolPtr pool $ \ poolPtr ->
                     do svnErr $ _open reposPtrPtr pathPtr poolPtr
                        wrapRepos pool =<< peek reposPtrPtr


createRepository :: FilePath -> Maybe (Hash Config) -> Maybe (Hash String) -> IO Repository
createRepository path config fsConfig
    = do pool <- newPool
         alloca $ \ reposPtrPtr ->
             withCString path $ \ pathPtr ->
                 withPoolPtr pool $ \ poolPtr ->
                     do svnErr (_create
                                reposPtrPtr
                                pathPtr
                                nullPtr
                                nullPtr
                                (unsafeHashToPtr' config)
                                (unsafeHashToPtr' fsConfig)
                                poolPtr)
                        repos <- wrapRepos pool =<< peek reposPtrPtr

                        -- config と fsConfig には、repos が死ぬまでは
                        -- 生きてゐて慾しい。
                        when (isJust config || isJust fsConfig)
                             $ GF.addForeignPtrConcFinalizer (case repos of Repository x -> x)
                                   $ do touchHash' config
                                        touchHash' fsConfig

                        return repos


deleteRepository :: FilePath -> IO ()
deleteRepository path
    = do pool <- newPool
         withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 svnErr $ _delete pathPtr poolPtr


getRepositoryFS :: Repository -> IO FileSystem
getRepositoryFS (Repository repos)
    = withForeignPtr repos $ \ reposPtr ->
      -- svn_fs_t* より先に svn_repos_t* が解放されては困る
      _fs reposPtr >>= wrapFS repos