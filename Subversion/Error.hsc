{- -*- haskell -*- -}

#include "HsSVN.h"

module Subversion.Error
    ( SvnError
    , SVN_ERROR_T

    , svnErrCode
    , svnErrMsg

    , svnErr
    )
    where

import           Control.Exception
import           Data.Dynamic
import           Foreign
import           Foreign.C

newtype SvnError
    = SvnError (ForeignPtr SVN_ERROR_T)
      deriving (Typeable)

data SVN_ERROR_T

type APR_SIZE_T   = #type apr_size_t
type APR_STATUS_T = #type apr_status_t


foreign import ccall "svn_err_best_message"
        _best_message :: Ptr SVN_ERROR_T -> Ptr CChar -> APR_SIZE_T -> IO (Ptr CChar)

foreign import ccall "&svn_error_clear"
        _clear :: FunPtr (Ptr SVN_ERROR_T -> IO ())


maxErrMsgLen :: Int
maxErrMsgLen = 255


withSvnErrorPtr :: SvnError -> (Ptr SVN_ERROR_T -> IO a) -> IO a
withSvnErrorPtr (SvnError err) = withForeignPtr err


svnErrCode :: SvnError -> IO SvnErrCode
svnErrCode err
    = withSvnErrorPtr err $ \ errPtr -> 
      do num <- (#peek svn_error_t, apr_err) errPtr
         return $ statusToErrCode num


svnErrMsg :: SvnError -> IO String
svnErrMsg err
    = withSvnErrorPtr err $ \ errPtr ->
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
           Just e  -> throwIO $ DynException $ toDyn e


data SvnErrCode
    = ReposLocked
    | UnknownError !Int
      deriving (Show, Eq, Typeable)

statusToErrCode :: APR_STATUS_T -> SvnErrCode
statusToErrCode (#const SVN_ERR_REPOS_LOCKED) = ReposLocked
statusToErrCode n                             = UnknownError (fromIntegral n)
