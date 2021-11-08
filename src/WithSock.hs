module WithSock where

import           Network.Run.TCP
import           Network.Socket
import           System.IO
import           Text.Printf

---- API ---

app :: IO ()
app = do
    printf "Listening on port: %s \n"  portNmb
    runTCPServer Nothing portNmb talk

--- Helpers ---

talk :: Socket -> IO ()
talk sock = do
    h <- socketToHandle sock ReadWriteMode
    hPutStrLn h "Welcome to qadratic server, please pass a number or quit to exit"
    hSetBuffering h LineBuffering
    loop h
  where
    loop :: Handle -> IO ()
    loop h = do
       line <- hGetLine h
       if line == quitCmd
         then do
            hPutStrLn h "Server exits!"
            hClose h
         else do
           let v = read line :: Integer
           hPutStrLn h $ "Result: " <> show (v * v)
           loop h

quitCmd = "quit"

portNmb :: String
portNmb = "4444"
