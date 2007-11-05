module Subversion.Stream
    ( Stream
    , SVN_STREAM_T

    , wrapStream
    , withStreamPtr

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
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.ForeignPtr   as GF
import           Subversion.Error
import           Subversion.Pool
import           Subversion.Types
import           System.IO.Unsafe


newtype Stream = Stream (ForeignPtr SVN_STREAM_T)
data SVN_STREAM_T


foreign import ccall unsafe "svn_stream_empty"
        _empty :: Ptr APR_POOL_T -> IO (Ptr SVN_STREAM_T)

foreign import ccall unsafe "svn_stream_for_stdout"
        _for_stdout :: Ptr (Ptr SVN_STREAM_T) -> Ptr APR_POOL_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_stream_read"
        _read :: Ptr SVN_STREAM_T -> Ptr CChar -> Ptr APR_SIZE_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_stream_write"
        _write :: Ptr SVN_STREAM_T -> Ptr CChar -> Ptr APR_SIZE_T -> IO (Ptr SVN_ERROR_T)

foreign import ccall unsafe "svn_stream_close"
        _close :: Ptr SVN_STREAM_T -> IO (Ptr SVN_ERROR_T)


wrapStream :: IO () -> Ptr SVN_STREAM_T -> IO Stream
wrapStream finalizer ioPtr
    = do io <- newForeignPtr_ ioPtr
         GF.addForeignPtrConcFinalizer io finalizer
         return $ Stream io


withStreamPtr :: Stream -> (Ptr SVN_STREAM_T -> IO a) -> IO a
withStreamPtr (Stream io) = withForeignPtr io


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
sReadLBS io = lazyRead >>= return . L8.fromChunks
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
sStrictReadLBS io = strictRead >>= return . L8.fromChunks
    where
      chunkSize = 32 * 1024

      strictRead = unsafeInterleaveIO loop

      loop = do bs <- sReadBS io chunkSize
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
