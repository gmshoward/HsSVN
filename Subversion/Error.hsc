{- -*- haskell -*- -}

{-# OPTIONS_HADDOCK prune #-}

-- |Common exception handling for Subversion. The C API of the
-- Subversion returns an error as a function result, but in HsSVN
-- errors are thrown as an 'Control.Exception.Exception'.

#include "HsSVN.h"

module Subversion.Error
    ( SvnError
    , SVN_ERROR_T -- private

    , wrapSvnError -- private

    , svnErrCode
    , svnErrMsg

    , svnErr -- private

    , SvnErrCode(..)
    )
    where

import           Control.Exception
import           Data.Dynamic
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Subversion.Types

-- |@'SvnError'@ represents a Subversion error.
newtype SvnError
    = SvnError (ForeignPtr SVN_ERROR_T)
      deriving (Typeable)

data SVN_ERROR_T

instance Show SvnError where
    show e = "SvnError " ++ show (svnErrCode e) ++ " " ++ show (svnErrMsg e)

instance Exception SvnError


foreign import ccall "svn_err_best_message"
        _best_message :: Ptr SVN_ERROR_T -> Ptr CChar -> APR_SIZE_T -> IO (Ptr CChar)

foreign import ccall "&svn_error_clear"
        _clear :: FunPtr (Ptr SVN_ERROR_T -> IO ())


maxErrMsgLen :: Int
maxErrMsgLen = 255


withSvnErrorPtr :: SvnError -> (Ptr SVN_ERROR_T -> IO a) -> IO a
withSvnErrorPtr (SvnError err) = withForeignPtr err

-- |@'svnErrCode' err@ returns a 'SvnErrCode' for an error object.
svnErrCode :: SvnError -> SvnErrCode
svnErrCode err
    = unsafePerformIO $
      withSvnErrorPtr err $ \ errPtr -> 
      do num <- (#peek svn_error_t, apr_err) errPtr
         return $ statusToErrCode num

-- |@'svnErrMsg' err@ returns an error message for an error object.
svnErrMsg :: SvnError -> String
svnErrMsg err
    = unsafePerformIO $
      withSvnErrorPtr err $ \ errPtr ->
      allocaArray maxErrMsgLen $ \ bufPtr ->
          _best_message errPtr bufPtr (fromIntegral maxErrMsgLen)
               >>= peekCString


wrapSvnError :: Ptr SVN_ERROR_T -> IO (Maybe SvnError)
wrapSvnError errPtr
    | errPtr == nullPtr
        = return Nothing
    | otherwise
        = fmap (Just . SvnError) (newForeignPtr _clear errPtr)


svnErr :: IO (Ptr SVN_ERROR_T) -> IO ()
svnErr f
    = do err <- wrapSvnError =<< f
         case err of
           Nothing -> return ()
           Just e  -> throwIO e

-- |@'SvnErrCode'@ represents a Subversion error code. As you see, not
-- all errors are translated to Haskell constructors yet. Uncovered
-- error codes are temporarily represented as @'UnknownError' num@.
data SvnErrCode
    = AprEEXIST         -- ^ APR EEXIST error: typically it means
                        --   something you tried to create was already
                        --   there.
    | AprENOENT         -- ^ APR ENOENT error: typically it means
                        --   something you tried to use wasn't there.
    | DirNotEmpty       -- ^ The directory needs to be empty but it's not.
    | ReposLocked       -- ^ The repository was locked, perhaps for db
                        --   recovery.
    | FsAlreadyExists   -- ^ The item already existed in filesystem.
    | FsConflict        -- ^ Merge conflict has occured during commit.
    | FsNoSuchRevision  -- ^ It was an invalid filesystem revision
                        --   number.
    | FsNotDirectory    -- ^ It was not a filesystem directory entry.
    | FsNotFile         -- ^ It was not a filesystem file entry.
    | FsNotFound        -- ^ It wasn't there in filesystem.
    | UnknownError !Int -- ^ Any other errors than above. You
                        --   shouldn't rely on the absence of
                        --   appropriate 'SvnErrCode' constructors
                        --   because they may be added in the future
                        --   version of HsSVN. If that happens to you,
                        --   your code stops working.
      deriving (Show, Eq, Typeable)

statusToErrCode :: APR_STATUS_T -> SvnErrCode
statusToErrCode (#const APR_EEXIST                 ) = AprEEXIST
statusToErrCode (#const APR_ENOENT                 ) = AprENOENT
statusToErrCode (#const SVN_ERR_DIR_NOT_EMPTY      ) = DirNotEmpty
statusToErrCode (#const SVN_ERR_REPOS_LOCKED       ) = ReposLocked
statusToErrCode (#const SVN_ERR_FS_ALREADY_EXISTS  ) = FsAlreadyExists
statusToErrCode (#const SVN_ERR_FS_CONFLICT        ) = FsConflict
statusToErrCode (#const SVN_ERR_FS_NO_SUCH_REVISION) = FsNoSuchRevision
statusToErrCode (#const SVN_ERR_FS_NOT_DIRECTORY   ) = FsNotDirectory
statusToErrCode (#const SVN_ERR_FS_NOT_FILE        ) = FsNotFile
statusToErrCode (#const SVN_ERR_FS_NOT_FOUND       ) = FsNotFound
statusToErrCode n                                    = UnknownError (fromIntegral n)
