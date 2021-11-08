module ParReq where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text)
import           Data.Time
import           Network.Wreq
import           Text.Printf

data HttpException = TimeoutException
                   | BadReqException
                   deriving Show

instance Exception HttpException

--- MAIN API ---

app = do
  as <- forM urls (async . timedDownload)
  forM_ as wait

app' = do
    let download url = do
          r <- get url
          pure (url, r)

    as <- forM urls $ async . download
    (url, r) <- waitAny as
    printf "%s was first (%d bytes)\n" url $ B.length $ r ^. responseBody
    forM_ as wait

--- Helpers ---

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
    v <- newEmptyTMVarIO
    t <- forkFinally action $ atomically . putTMVar v
    pure $ Async t v

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ v) = atomically $ readTMVar v

waitAny :: [Async a] -> IO a
waitAny = atomically . foldr (orElse . waitSTM) retry

waitSTM :: Async a -> STM a
waitSTM a = do
    r <- waitCatchSTM a
    case r of
      Left e  -> throwSTM e
      Right a -> pure a

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ v) = readTMVar v

wait :: Async a -> IO a
wait v = do
    r <- waitCatch v
    case r of
      Left e  -> throwIO e
      Right a -> pure a

timedDownload :: String -> IO ()
timedDownload url = do
    (res, time) <- timeit $ get url
    let rSize = B.length $ res ^. responseBody
    printf "Downloaded: %s (%d bytes, %.2fs)\n" url rSize time

timeit :: IO a -> IO (a,Double)
timeit io = do
     t0 <- getCurrentTime
     a  <- io
     t1 <- getCurrentTime
     return (a, realToFrac (t1 `diffUTCTime` t0))

urls = [ "https://www.google.com"
       , "https://www.yahoo.com"
       , "https://www.bing.com"
       , urlBase
       ]

urlBase = "https://www.wikipedia.org/wiki/"
url1 = urlBase <> "Shovel"
url2 = urlBase <> "Spade"
