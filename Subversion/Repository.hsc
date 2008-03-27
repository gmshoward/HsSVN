{- -*- haskell -*- -}

-- |An interface to repository, which is built on top of the
-- filesystem.

#include "HsSVN.h"

module Subversion.Repository
    ( Repository

    , openRepository
    , createRepository
    , deleteRepository

    , getRepositoryFS

    , doReposTxn

    , dumpRepository
    )
    where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.Maybe
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr as GF
import           Subversion.Config
import           Subversion.FileSystem
import           Subversion.FileSystem.Transaction
import           Subversion.Hash
import           Subversion.Error
import           Subversion.Pool
import           Subversion.Stream
import           Subversion.Stream.Pipe
import           Subversion.Types

-- |@'Repository'@ is an opaque object representing a Subversion
-- repository.
newtype Repository = Repository (ForeignPtr SVN_REPOS_T)
data SVN_REPOS_T


type CancelFunc = Ptr () -> IO (Ptr SVN_ERROR_T)


foreign import ccall unsafe "svn_repos_open"
        _open :: Ptr (Ptr SVN_REPOS_T) -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_repos_create"
        _create :: Ptr (Ptr SVN_REPOS_T) -> CString -> Ptr CChar -> Ptr CChar -> Ptr APR_HASH_T -> Ptr APR_HASH_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_repos_delete"
        _delete :: CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_repos_fs"
        _fs :: Ptr SVN_REPOS_T -> IO (Ptr SVN_FS_T)

foreign import ccall unsafe "svn_repos_fs_begin_txn_for_commit"
        _fs_begin_txn_for_commit :: Ptr (Ptr SVN_FS_TXN_T) -> Ptr SVN_REPOS_T -> SVN_REVNUM_T -> CString -> CString -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_repos_fs_commit_txn"
        _fs_commit_txn :: Ptr CString -> Ptr SVN_REPOS_T -> Ptr SVN_REVNUM_T -> Ptr SVN_FS_TXN_T -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall safe "svn_repos_dump_fs2"
        _dump_fs2 :: Ptr SVN_REPOS_T      -- repos
                  -> Ptr SVN_STREAM_T     -- dumpstream
                  -> Ptr SVN_STREAM_T     -- feedback_stream
                  -> SVN_REVNUM_T         -- start_rev
                  -> SVN_REVNUM_T         -- end_rev
                  -> SVN_BOOLEAN_T        -- incremental
                  -> SVN_BOOLEAN_T        -- use_deltas
                  -> FunPtr CancelFunc    -- cancel_func
                  -> Ptr ()               -- cancel_baton
                  -> Ptr APR_POOL_T       -- pool
                  -> IO (Ptr SVN_ERROR_T)


wrapRepos :: IO () -> Ptr SVN_REPOS_T -> IO Repository
wrapRepos finalizer reposPtr
    = do repos <- newForeignPtr_ reposPtr
         GF.addForeignPtrConcFinalizer repos finalizer
         return $ Repository repos


withReposPtr :: Repository -> (Ptr SVN_REPOS_T -> IO a) -> IO a
withReposPtr (Repository repos) = withForeignPtr repos


touchRepos :: Repository -> IO ()
touchRepos (Repository repos) = touchForeignPtr repos

-- |@'openRepository' fpath@ opens a Subversion repository at @fpath@.
--
-- It acquires a shared lock on the repository, and the lock will be
-- removed by the garbage collector. If an exclusive lock is present,
-- this blocks until it's gone.
openRepository :: FilePath -> IO Repository
openRepository path
    = do pool <- newPool
         alloca $ \ reposPtrPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 do svnErr $ _open reposPtrPtr pathPtr poolPtr
                    wrapRepos (touchPool pool) =<< peek reposPtrPtr

-- |@'createRepository'@ creates a new Subversion repository, building
-- the necessary directory structure, creating filesystem, and so on.
createRepository
    :: FilePath           -- ^ Where to create the repository.
    -> [(String, Config)] -- ^ A list of @(categoryName, config)@
                          --   tuples which represents a client
                          --   configuration. It may be an empty list.
    -> [(String, String)] -- ^ This list is passed to the
                          --   filesystem. See
                          --   'Subversion.FileSystem.createFileSystem'.
    -> IO Repository
createRepository path configPairs fsConfigPairs
    = do pool <- newPool
         alloca $ \ reposPtrPtr ->
             withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 do config   <- pairsToHash configPairs
                    fsConfig <- pairsToHash fsConfigPairs
                    svnErr (_create
                            reposPtrPtr
                            pathPtr
                            nullPtr
                            nullPtr
                            (unsafeHashToPtr config)
                            (unsafeHashToPtr fsConfig)
                            poolPtr)

                    repos <- wrapRepos (touchPool pool) =<< peek reposPtrPtr

                    -- config と fsConfig には、repos が死ぬまでは生き
                    -- てゐて慾しい。
                    GF.addForeignPtrConcFinalizer (case repos of Repository x -> x)
                          $ (touchHash config >> touchHash fsConfig)

                    return repos

-- |@'deleteRepository' fpath@ destroys the Subversion repository at
-- @fpath@.
deleteRepository :: FilePath -> IO ()
deleteRepository path
    = do pool <- newPool
         withCString path $ \ pathPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 svnErr $ _delete pathPtr poolPtr

-- |@'getRepositoryFS' repos@ returns the filesystem associated with
-- repository @repos@.
getRepositoryFS :: Repository -> IO FileSystem
getRepositoryFS repos
    = withReposPtr repos $ \ reposPtr ->
      -- svn_fs_t* より先に svn_repos_t* が解放されては困る
      _fs reposPtr >>= wrapFS (touchRepos repos)


beginTxn :: Repository -> RevNum -> String -> Maybe String -> IO Transaction
beginTxn repos revNum author logMsg
    = do pool <- newPool
         alloca $ \ txnPtrPtr ->
             withReposPtr repos  $ \ reposPtr  ->
             withCString  author $ \ authorPtr ->
             withCString' logMsg $ \ logMsgPtr ->
             withPoolPtr  pool   $ \ poolPtr   ->
             (svnErr $
              _fs_begin_txn_for_commit
              txnPtrPtr
              reposPtr
              (fromIntegral revNum)
              authorPtr
              logMsgPtr
              poolPtr)
             >>  peek txnPtrPtr
             >>= (wrapTxn $
                  -- txn は pool にも repos にも依存する。
                  touchPool pool >> touchRepos repos)
    where
      withCString' :: Maybe String -> (CString -> IO a) -> IO a
      withCString' Nothing    f = f nullPtr
      withCString' (Just str) f = withCString str f
             

commitTxn :: Repository -> Transaction -> IO (Either FilePath RevNum)
commitTxn repos txn
    = do pool <- newPool
         alloca $ \ conflictPathPtrPtr ->
             withReposPtr repos $ \ reposPtr  ->
             alloca             $ \ newRevPtr ->
             withTxnPtr  txn    $ \ txnPtr    ->
             withPoolPtr pool   $ \ poolPtr   ->
             do err <- wrapSvnError =<< (_fs_commit_txn
                                         conflictPathPtrPtr
                                         reposPtr
                                         newRevPtr
                                         txnPtr
                                         poolPtr)
                case err of
                  Nothing
                      -> liftM (Right . fromIntegral) (peek newRevPtr)

                  Just e
                      -> if svnErrCode e == FsConflict then
                             return . Left =<< peekCString =<< peek conflictPathPtrPtr
                         else
                             throwSvnErr e

-- |@'doReposTxn'@ tries to do the transaction. If it succeeds
-- 'doReposTxn' automatically commits it, but if it throws an
-- exception 'doReposTxn' automatically cancels it and rethrow the
-- exception.
--
-- Because conflicts tend to occur more frequently than other errors,
-- they aren't reported as an exception.
doReposTxn
    :: Repository   -- ^ The repository.
    -> RevNum       -- ^ An existing revision number which the
                    --   transaction bases on.
    -> String       -- ^ The author name to be recorded as a
                    --   transaction property.
    -> Maybe String -- ^ The log message to be recorded as a
                    --   transaction property. This value may be
                    --   'Prelude.Nothing' if the message is not yet
                    --   available. The caller will need to attach one
                    --   to the transaction at a later time.
    -> Txn ()       -- ^ The transaction to be done.
    -> IO (Either FilePath RevNum) -- ^ The result is whether
                                   -- @'Prelude.Left' conflictPath@
                                   -- (if it conflicted) or
                                   -- @'Prelude.Right' newRevNum@ (if
                                   -- it didn't).
doReposTxn repos revNum author logMsg c
    = do txn <- beginTxn repos revNum author logMsg
         handle (cleanUp txn) (tryTxn txn)
    where
      cleanUp :: Transaction -> Exception -> IO a
      cleanUp txn exn
          = abortTxn txn
            >>
            throwIO exn

      tryTxn :: Transaction -> IO (Either FilePath Int)
      tryTxn txn
          = do runTxn c txn
               
               -- Good. We've got no exceptions during the computation
               -- of Txn (). Now let us commit the transaction.
               commitTxn repos txn


-- |Lazily dump the contents of the filesystem within already-open
-- repository.
dumpRepository :: Repository   -- ^ The repository.
               -> Maybe RevNum -- ^ @'Prelude.Nothing'@ to start
                               -- dumping at revision 0, or
                               -- @'Prelude.Just' x@ to begin at
                               -- revision @x@.
               -> Maybe RevNum -- ^ @'Prelude.Nothing'@ to dump
                               -- through the HEAD revision, or
                               -- @'Prelude.Just' x@ to dump up
                               -- through revision @x@.
               -> Bool         -- ^ If this is @'Prelude.True'@, the
                               -- first revision dumped will be a diff
                               -- against the previous revision
                               -- (usually it looks like a full dump
                               -- of the tree).
               -> Bool         -- ^ If this is @'Prelude.True'@,
                               -- output only node properties which
                               -- have changed relative to the
                               -- previous contents, and output text
                               -- contents as svndiff data against the
                               -- previous contents. Regardless of how
                               -- this flag is set, the first revision
                               -- of a non-incremental dump will be
                               -- done with full plain text. A dump
                               -- with this flag set cannot be loaded
                               -- by Subversion 1.0.x.
               -> IO Lazy.ByteString
dumpRepository repos startRev endRev incremental useDeltas
    = do pool <- newPool
         pipe <- newPipe
         forkIO $ do withReposPtr repos $ \ reposPtr ->
                         withStreamPtr pipe $ \ pipePtr ->
                         withPoolPtr pool $ \ poolPtr ->
                         svnErr $ _dump_fs2 reposPtr
                                            pipePtr
                                            nullPtr
                                            (fromMaybe invalidRevNum $ fmap fromIntegral startRev)
                                            (fromMaybe invalidRevNum $ fmap fromIntegral endRev)
                                            (marshalBool incremental)
                                            (marshalBool useDeltas)
                                            nullFunPtr
                                            nullPtr
                                            poolPtr
                     sClose pipe
         sReadLBS pipe
    where
      invalidRevNum :: SVN_REVNUM_T
      invalidRevNum = #const SVN_INVALID_REVNUM