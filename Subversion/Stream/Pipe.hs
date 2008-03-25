module Subversion.Stream.Pipe
    ( newPipe 
    )
    where

import           Control.Concurrent.STM
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Subversion.Stream


data Pipe
    = Pipe {
        pReadRequest :: TVar Int             -- 讀込み側が要求し、未だ書込まれてゐないバイト數
      , pWrittenStr  :: TVar Lazy.ByteString -- 要求に應へて書込まれ、未だ讀込まれてゐない文字列
      , pIsClosed    :: TVar Bool            -- パイプが閉ぢられた
      }


newPipe :: IO Stream
newPipe = do req <- newTVarIO 0
             str <- newTVarIO Lazy.empty
             isC <- newTVarIO False
             
             let pipe    = Pipe {
                             pReadRequest = req
                           , pWrittenStr  = str
                           , pIsClosed    = isC
                           }
                 actions = StreamActions {
                             saRead  = mkReadAction  pipe
                           , saWrite = mkWriteAction pipe
                           , saClose = mkCloseAction pipe
                           }

             newStream actions


mkReadAction :: Pipe -> ReadAction
mkReadAction pipe reqLen
    = atomically $
      do str <- readTVar (pWrittenStr pipe)
         if Lazy.null str then
             -- 書込まれた文字列が無いので、要求されたバイト數をパイプ
             -- に書いて retry する。但しパイプが閉ぢられてゐたら空文字
             -- 列を返して EOF を示す。
             do isClosed <- readTVar (pIsClosed pipe)
                if isClosed then
                    return Strict.empty
                  else
                    do oldReq <- readTVar (pReadRequest pipe)
                       writeTVar (pReadRequest pipe) (oldReq + reqLen)
                       retry
           else
             -- reqLen バイトを上限としてバッファの頭を切り取る。
             do let (readStr, remaining) = Lazy.splitAt (fromIntegral reqLen) str
                writeTVar (pWrittenStr pipe) remaining
                return (Strict.concat (Lazy.toChunks readStr))


mkWriteAction :: Pipe -> WriteAction
mkWriteAction pipe str
    = atomically $
      do let inputLen = Strict.length str
         isClosed <- readTVar (pIsClosed pipe)
         if isClosed then
             -- パイプが閉ぢられてゐたら書込まれた文字列を捨てる
             -- FIXME: 本當にそれで良いのか？
             return inputLen
           else
             do requestedBytes <- readTVar (pReadRequest pipe)
                writtenStr     <- readTVar (pWrittenStr pipe)
                let maxBytesToWrite = requestedBytes - (fromIntegral $ Lazy.length writtenStr)
                if maxBytesToWrite > 0 then
                    -- 要求されてゐるので書込む
                    do let strToWrite   = Strict.take maxBytesToWrite str
                           bytesToWrite = Strict.length strToWrite
                       writeTVar (pReadRequest pipe) (requestedBytes - bytesToWrite)
                       writeTVar (pWrittenStr pipe) (writtenStr `Lazy.append` (Lazy.fromChunks [strToWrite]))
                       return bytesToWrite
                  else
                    -- 要求されてゐないので retry
                    retry


mkCloseAction :: Pipe -> CloseAction
mkCloseAction pipe
    = atomically $
      writeTVar (pIsClosed pipe) True