module AsLogger where

import           Control.Concurrent
import           Control.Monad

class AsLogger a where
    logInit :: IO a
    logMsg  :: a -> String -> IO ()
    stop    :: a -> IO ()

instance AsLogger Logger where
  logInit :: IO Logger
  logInit = do
    newMV <- newEmptyMVar
    let l = Logger newMV
    forkIO (logger l)
    pure l

  logMsg :: Logger -> String -> IO ()
  logMsg (Logger m) = putMVar m . Msg

  stop :: Logger -> IO ()
  stop (Logger m) = do
    s <- newEmptyMVar
    putMVar m (Stop s)
    takeMVar s

newtype Logger = Logger (MVar LogMsg)

data LogMsg = Msg String
            | Stop (MVar ())

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      msg <- takeMVar m
      case msg of
        Msg msg -> putStrLn ("Logger: " <> msg) >> loop
        Stop s  -> putStrLn "Logger: stopped" >> putMVar s ()

trdCnt = [1..1000]

app :: IO ()
app = do
    l <- (logInit :: IO Logger)
    forM_ trdCnt $ \i -> forkIO $ logMsg l ("Hello from: " <> show i)
    logMsg l "Exiting main thread"
    stop l

