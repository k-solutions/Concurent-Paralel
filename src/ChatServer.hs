module ChatServer where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8     as S
import           Network.Run.TCP
import           Network.Socket
import           Network.Socket.ByteString
import           System.IO

import           Control.Monad.Fix         (fix)

type Msg = (Int, S.ByteString)

app :: IO ()
app = do
    chan <- newChan
    void $ forkIO $ fix $ \loop -> do
      (_, msg) <- readChan chan
      print $ "Message received: " <> msg
      loop
    runTCPServer Nothing "4242" $ mainLoop 0 chan

mainLoop :: Int -> Chan Msg -> Socket -> IO ()
mainLoop msgNum chan sock = do
  void . forkIO $ withMsg msgNum chan sock
  let !newNum = msgNum + 1
  mainLoop newNum chan sock

withMsg :: Int -> Chan Msg -> Socket -> IO ()
withMsg msgNum chan sock = do
    let broadcast msg = writeChan chan (msgNum, msg)
    msgHandle <- socketToHandle sock ReadWriteMode
    hSetBuffering msgHandle NoBuffering

    --- Start chat and username ---
    S.hPutStrLn msgHandle "Welcome to the chat. Please choose username: "
    usrName <- S.init <$> S.hGetLine msgHandle
    broadcast ("--> " <> usrName <> " is online.")
    S.hPutStrLn msgHandle ("Hello, " <> usrName <> "!")

    comLine <- dupChan chan

    -- Fork tread which will read mesages from the duplicated channel
    readerFromDupChan <- forkIO $ fix $ \loop -> do
      S.hPutStrLn msgHandle "Message nmb is:"
      (nxtNum, newMsg) <- readChan comLine
      when (msgNum /= nxtNum) $ S.hPutStrLn msgHandle newMsg
      loop

    handle (\(SomeException  _) -> pure ()) $ fix $ \loop -> do
      msg <- S.init <$> S.hGetLine msgHandle
      case msg of
        "quit" -> S.hPutStrLn msgHandle "Buy!"                -- on exception brake the loop
        _      -> broadcast (usrName <> ": " <> msg) >> loop  -- otherwise continue

    killThread readerFromDupChan                              -- Lets kill thread after loop is ended
    broadcast ("<--" <> usrName <> "is now offline.")         -- and we are ready to close
    hClose msgHandle


