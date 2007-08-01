{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.Error
    ( SvnError
    , SVN_ERROR_T

    , wrapSvnError

    , svnErrCode
    , svnErrMsg

    , svnErr

    , throwSvnErr

    , SvnErrCode(..)
    )
    where

import           Control.Exception
import           Data.Dynamic
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Subversion.Types


newtype SvnError
    = SvnError (ForeignPtr SVN_ERROR_T)
      deriving (Typeable)

data SVN_ERROR_T


foreign import ccall "svn_err_best_message"
        _best_message :: Ptr SVN_ERROR_T -> Ptr CChar -> APR_SIZE_T -> IO (Ptr CChar)

foreign import ccall "&svn_error_clear"
        _clear :: FunPtr (Ptr SVN_ERROR_T -> IO ())


maxErrMsgLen :: Int
maxErrMsgLen = 255


withSvnErrorPtr :: SvnError -> (Ptr SVN_ERROR_T -> IO a) -> IO a
withSvnErrorPtr (SvnError err) = withForeignPtr err


svnErrCode :: SvnError -> SvnErrCode
svnErrCode err
    = unsafePerformIO $
      withSvnErrorPtr err $ \ errPtr -> 
      do num <- (#peek svn_error_t, apr_err) errPtr
         return $ statusToErrCode num


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
        = newForeignPtr _clear errPtr >>= return . Just . SvnError


svnErr :: IO (Ptr SVN_ERROR_T) -> IO ()
svnErr f
    = do err <- wrapSvnError =<< f
         case err of
           Nothing -> return ()
           Just e  -> throwSvnErr e


throwSvnErr :: SvnError -> IO a
throwSvnErr = throwIO . DynException . toDyn


data SvnErrCode
    = AprEEXIST
    | AprENOENT
    | ReposLocked
    | FsAlreadyExists
    | FsConflict
    | FsNoSuchRevision
    | FsNotDirectory
    | FsNotFile
    | FsNotFound
    | UnknownError !Int
      deriving (Show, Eq, Typeable)

statusToErrCode :: APR_STATUS_T -> SvnErrCode
statusToErrCode (#const APR_EEXIST                 ) = AprEEXIST
statusToErrCode (#const APR_ENOENT                 ) = AprENOENT
statusToErrCode (#const SVN_ERR_REPOS_LOCKED       ) = ReposLocked
statusToErrCode (#const SVN_ERR_FS_ALREADY_EXISTS  ) = FsAlreadyExists
statusToErrCode (#const SVN_ERR_FS_CONFLICT        ) = FsConflict
statusToErrCode (#const SVN_ERR_FS_NO_SUCH_REVISION) = FsNoSuchRevision
statusToErrCode (#const SVN_ERR_FS_NOT_DIRECTORY   ) = FsNotDirectory
statusToErrCode (#const SVN_ERR_FS_NOT_FILE        ) = FsNotFile
statusToErrCode (#const SVN_ERR_FS_NOT_FOUND       ) = FsNotFound
statusToErrCode n                                    = UnknownError (fromIntegral n)
