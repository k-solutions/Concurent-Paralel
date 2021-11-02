module ChatServer where

-- import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString           as S
import           Network.Run.TCP
import           Network.Socket
import           Network.Socket.ByteString

type Message = S.ByteString

app :: IO ()
app = runTCPServer Nothing "8080" mainLoop

mainLoop ::  Socket -> IO ()
mainLoop sock = do
    msg <- recv sock 1024
    if S.null msg
      then withMsg (sock, msg)
      else mainLoop sock

withMsg :: (Socket, Message) -> IO ()
withMsg (sock, _) = do
    void $ sendAll sock "Hello World!\n"
    close sock
