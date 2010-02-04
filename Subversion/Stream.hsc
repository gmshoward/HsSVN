module Subversion.Stream
    ( Stream
    , SVN_STREAM_T

    , ReadAction
    , WriteAction
    , CloseAction
    , StreamActions(..)

    , wrapStream
    , withStreamPtr

    , newStream
    , getEmptyStream
    , getStreamForStdOut

    , sRead
    , sReadBS
    , sReadLBS

    , sStrictRead
    , sStrictReadLBS

    , sWrite
    , sWriteBS
    , sWriteLBS

    , sClose
    )
    where

import           Control.Monad
import qualified Data.ByteString            as Strict        (ByteString)
import qualified Data.ByteString.Char8      as B8     hiding (ByteString)
import qualified Data.ByteString.Lazy       as Lazy          (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8     hiding (ByteString)
import           Data.ByteString.Internal             hiding (ByteString)
import           Data.ByteString.Unsafe
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.Error
import           Subversion.Pool
import           Subversion.Types
import           System.IO.Unsafe


newtype Stream = Stream (ForeignPtr SVN_STREAM_T)
data SVN_STREAM_T


type ReadAction  = Int -> IO Strict.ByteString
type WriteAction = Strict.ByteString -> IO Int
type CloseAction = IO ()


data StreamActions
    = StreamActions {
        saRead  :: !ReadAction
      , saWrite :: !WriteAction
      , saClose :: !CloseAction
      }


type ReadCallback  = Ptr () -> CString -> Ptr APR_SIZE_T -> IO (Ptr SVN_ERROR_T)
type WriteCallback = Ptr () -> CString -> Ptr APR_SIZE_T -> IO (Ptr SVN_ERROR_T)
type CloseCallback = Ptr () -> IO (Ptr SVN_ERROR_T)


foreign import ccall unsafe "svn_stream_create"
        _create :: Ptr () -> Ptr APR_POOL_T -> IO (Ptr SVN_STREAM_T)

foreign import ccall unsafe "svn_stream_set_read"
        _set_read :: Ptr SVN_STREAM_T -> FunPtr ReadCallback -> IO ()

foreign import ccall unsafe "svn_stream_set_write"
        _set_write :: Ptr SVN_STREAM_T -> FunPtr WriteCallback -> IO ()

foreign import ccall unsafe "svn_stream_set_close"
        _set_close :: Ptr SVN_STREAM_T -> FunPtr CloseCallback -> IO ()

foreign import ccall unsafe "svn_stream_empty"
        _empty :: Ptr APR_POOL_T -> IO (Ptr SVN_STREAM_T)

foreign import ccall unsafe "svn_stream_for_stdout"
        _for_stdout :: Ptr (Ptr SVN_STREAM_T) -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall safe "svn_stream_read"
        _read :: Ptr SVN_STREAM_T -> Ptr CChar -> Ptr APR_SIZE_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall safe "svn_stream_write"
        _write :: Ptr SVN_STREAM_T -> Ptr CChar -> Ptr APR_SIZE_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall safe "svn_stream_close"
        _close :: Ptr SVN_STREAM_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall "wrapper"
        mkReadCallback :: ReadCallback -> IO (FunPtr ReadCallback)

foreign import ccall "wrapper"
        mkWriteCallback :: WriteCallback -> IO (FunPtr WriteCallback)

foreign import ccall "wrapper"
        mkCloseCallback :: CloseCallback -> IO (FunPtr CloseCallback)


wrapStream :: IO () -> Ptr SVN_STREAM_T -> IO Stream
wrapStream finalizer ioPtr
    = do io <- newForeignPtr_ ioPtr
         GF.addForeignPtrConcFinalizer io finalizer
         return $ Stream io


withStreamPtr :: Stream -> (Ptr SVN_STREAM_T -> IO a) -> IO a
withStreamPtr (Stream io) = withForeignPtr io


newStream :: StreamActions -> IO Stream
newStream actions
    = do pool <- newPool
         withPoolPtr pool $ \ poolPtr ->
             do streamPtr <- _create nullPtr poolPtr
                
                readFnPtr  <- mkReadFnPtr (saRead actions)
                writeFnPtr <- mkWriteFnPtr (saWrite actions)
                closeFnPtr <- mkCloseFnPtr (saClose actions)
                
                _set_read streamPtr readFnPtr
                _set_write streamPtr writeFnPtr
                _set_close streamPtr closeFnPtr

                wrapStream (do freeHaskellFunPtr readFnPtr
                               freeHaskellFunPtr writeFnPtr
                               freeHaskellFunPtr closeFnPtr
                               touchPool pool
                           ) streamPtr
    where
      mkReadFnPtr :: ReadAction -> IO (FunPtr ReadCallback)
      mkReadFnPtr ra
          = mkReadCallback $ \ _ bufPtr lenPtr ->
            do requestedLen <- liftM fromIntegral (peek lenPtr)
               resultStr    <- ra requestedLen -- FIXME: should catch exceptions
               B8.useAsCStringLen resultStr $ \ (resultPtr, resultLen) ->
                   do when (resultLen > requestedLen)
                           $ fail "resultLen > requestedLen" -- FIXME
                      copyArray bufPtr resultPtr resultLen
                      poke lenPtr (fromIntegral resultLen)
               return nullPtr

      mkWriteFnPtr :: WriteAction -> IO (FunPtr WriteCallback)
      mkWriteFnPtr wa
          = mkWriteCallback $ \ _ bufPtr lenPtr ->
            do requestedLen <- liftM fromIntegral (peek lenPtr)
               inputStr     <- B8.packCStringLen (bufPtr, requestedLen)
               writtenLen   <- wa inputStr -- FIXME: should catch exceptions
               poke lenPtr (fromIntegral writtenLen)
               return nullPtr

      mkCloseFnPtr :: CloseAction -> IO (FunPtr CloseCallback)
      mkCloseFnPtr ca
          = mkCloseCallback $ \ _ ->
            do ca -- FIXME: should catch exceptions
               return nullPtr


getEmptyStream :: IO Stream
getEmptyStream 
    = do pool <- newPool
         withPoolPtr pool $ \ poolPtr ->
             wrapStream (touchPool pool) =<< _empty poolPtr


getStreamForStdOut :: IO Stream
getStreamForStdOut
    = do pool <- newPool
         alloca $ \ ioPtrPtr ->
             withPoolPtr pool $ \ poolPtr ->
                 do svnErr $ _for_stdout ioPtrPtr poolPtr
                    wrapStream (touchPool pool) =<< peek ioPtrPtr


sRead :: Stream -> IO String
sRead io
    = liftM L8.unpack $ sReadLBS io


sStrictRead :: Stream -> IO String
sStrictRead io
    = liftM L8.unpack $ sStrictReadLBS io


sReadBS :: Stream -> Int -> IO Strict.ByteString
sReadBS io maxLen
    = withStreamPtr io     $ \ ioPtr     ->
      createAndTrim maxLen $ \ bufPtr    ->
      alloca               $ \ bufLenPtr ->
      do poke bufLenPtr (fromIntegral maxLen)
         svnErr $ _read ioPtr (castPtr bufPtr) bufLenPtr
         liftM fromIntegral $ peek bufLenPtr


sReadLBS :: Stream -> IO Lazy.ByteString
sReadLBS io = fmap L8.fromChunks lazyRead
    where
      chunkSize = 32 * 1024

      lazyRead = unsafeInterleaveIO loop

      loop = do bs <- sReadBS io chunkSize
                if B8.null bs then
                    -- reached EOF
                    return []
                  else
                    do bss <- lazyRead
                       return (bs:bss)


sStrictReadLBS :: Stream -> IO Lazy.ByteString
sStrictReadLBS io = fmap L8.fromChunks strictRead
    where
      chunkSize = 32 * 1024

      strictRead = do bs <- sReadBS io chunkSize
                      if B8.null bs then
                          -- reached EOF
                          return []
                        else
                          do bss <- strictRead
                             return (bs:bss)



sWrite :: Stream -> String -> IO ()
sWrite io str
    = sWriteLBS io (L8.pack str)


sWriteBS :: Stream -> Strict.ByteString -> IO ()
sWriteBS io str
    = withStreamPtr io $ \ ioPtr ->
      unsafeUseAsCStringLen str $ \ (strPtr, strLen) ->
      alloca $ \ strLenPtr ->
          do poke strLenPtr (fromIntegral strLen)
             svnErr $ _write ioPtr strPtr strLenPtr


sWriteLBS :: Stream -> Lazy.ByteString -> IO ()
sWriteLBS io chunks
    = mapM_ (sWriteBS io) (L8.toChunks chunks)


sClose :: Stream -> IO ()
sClose io
    = withStreamPtr io $ \ ioPtr ->
      svnErr $ _close ioPtr
