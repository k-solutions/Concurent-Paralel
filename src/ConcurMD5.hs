{-# LANGUAGE BangPatterns #-}

module ConcurMD5 where

import           Control.Concurrent
import           Control.Monad        (replicateM_)
import qualified Data.ByteString.Lazy as L
import           Data.Digest.Pure.MD5 (md5)
import           Data.Functor         ((<&>))

app :: [FilePath] -> IO ()
app files = do
    str <- newEmptyMVar
    mapM_ (forkIO . hashAndPrint str) files
    printNrResults (length files) str
app _ = putStrLn "We need two file paths as arguments"

--- Helpers ---

printNrResults :: Int -> MVar String -> IO ()
printNrResults i v = replicateM_ i (takeMVar v >>= putStrLn)

hashAndPrint :: MVar String -> FilePath -> IO ()
hashAndPrint str f = do
  bs <- L.readFile f
  let !h = show $ md5 bs
  putMVar str (f <> ": " <> h)
