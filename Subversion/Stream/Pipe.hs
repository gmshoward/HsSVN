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
        pWrittenStr  :: TVar Lazy.ByteString -- 書き込まれたが、未だ讀込まれてゐない文字列
      , pIsClosed    :: TVar Bool            -- パイプが閉ぢられた
      }


newPipe :: IO Stream
newPipe = do str <- newTVarIO Lazy.empty
             isC <- newTVarIO False
             
             let pipe    = Pipe {
                             pWrittenStr  = str
                           , pIsClosed    = isC
                           }
                 actions = StreamActions {
                             saRead  = mkReadAction  pipe
                           , saWrite = mkWriteAction pipe
                           , saClose = mkCloseAction pipe
                           }

             newStream actions


mkReadAction :: Pipe -> ReadAction
mkReadAction pipe reqLen = loop
    where
      loop :: IO Strict.ByteString
      loop = do nextAction <- tryToRead
                nextAction

      tryToRead :: IO (IO Strict.ByteString)
      tryToRead
          = atomically $
            do str <- readTVar (pWrittenStr pipe)
               if Lazy.length str < fromIntegral reqLen then
                   -- 書込まれた文字列が足りないので、要求されたバイト
                   -- 數をパイプに書いて書込みを待つ。但しパイプが閉ぢ
                   -- られてゐたら reqLen に滿たない長さの文字列を返し
                   -- て EOF を示す。
                   do isClosed <- readTVar (pIsClosed pipe)
                      if isClosed then
                          do writeTVar (pWrittenStr pipe) Lazy.empty
                             return (return (Strict.concat (Lazy.toChunks str)))
                        else
                          retry
                 else
                   -- reqLen バイトを上限としてバッファの頭を切り取る。
                   do let (readStr, remaining) = Lazy.splitAt (fromIntegral reqLen) str
                      writeTVar (pWrittenStr pipe) remaining
                      return (return (Strict.concat (Lazy.toChunks readStr)))


mkWriteAction :: Pipe -> WriteAction
mkWriteAction pipe str
    = atomically $
      do let inputLen = Strict.length str
         isClosed <- readTVar (pIsClosed pipe)
         if isClosed then
             -- パイプが閉ぢられてゐたら書込まれた文字列を捨てる
             -- FIXME: 本當にそれで良いのか？
             return ()
           else
             do writtenStr <- readTVar (pWrittenStr pipe)
                writeTVar (pWrittenStr pipe) (writtenStr `Lazy.append` (Lazy.fromChunks [str]))
         return inputLen


mkCloseAction :: Pipe -> CloseAction
mkCloseAction pipe
    = atomically $
      writeTVar (pIsClosed pipe) True