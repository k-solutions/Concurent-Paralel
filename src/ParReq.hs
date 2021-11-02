module ParReq where

import           Control.Concurrent
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import           Data.Text            (Text)
import           Network.Wreq

app = do
    a1 <- async (get url1)
    a2 <- async (get url2)

    r1 <- wait a1
    r2 <- wait a2

    let r1Body = r1 ^. responseBody
        r2Body = r2 ^. responseBody

    print (B.length r1Body, B.length r2Body)


--- Helpers ---

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
    v <- newEmptyMVar
    forkIO $ action >>= putMVar v
    pure $ Async v

wait :: Async a -> IO a
wait (Async v) = readMVar v

urlBase = "https://www.wikipedia.org/wiki/"
url1 = urlBase <> "Shovel"
url2 = urlBase <> "Spade"
