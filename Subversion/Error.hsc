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


foreign import ccall "svn_err_best_message"
        _best_message :: Ptr SVN_ERROR_T -> Ptr CChar -> CSize -> IO (Ptr CChar)

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
         return $ intToErrCode num


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

intToErrCode :: Int -> SvnErrCode
intToErrCode (#const SVN_ERR_REPOS_LOCKED) = ReposLocked
intToErrCode n                             = UnknownError n
