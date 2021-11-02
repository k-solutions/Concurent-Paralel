module PhoneBook where

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Functor         ((<&>))
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as Tx
import           Prelude              hiding (lookup)
import           Text.Printf

type Name      = Text
type Phone     = Text
type PhoneBook = Map Name Phone

newtype PBState = PBState (MVar PhoneBook)

class HasPhoneBook a where
    new    :: IO a
    insert :: a -> Name -> Phone -> IO ()
    lookup :: a -> Name -> IO (Maybe Phone)

instance HasPhoneBook PBState where
    new :: IO PBState
    new = newMVar M.empty <&> PBState

    insert :: PBState -> Name -> Phone -> IO ()
    insert (PBState state) name phnNmb = do  --- Strict evaluation of M.insert and short lock optimisation
                b <- takeMVar state
                let b' = M.insert name phnNmb b
                putMVar state b'
                seq b' (return ())

    lookup :: PBState -> Name -> IO (Maybe Phone)
    lookup (PBState state) name = do
        b <- takeMVar state
        putMVar state b
        pure $ M.lookup name b

maxNmbs = map (Tx.pack . show) [1..100000]

app :: [String] -> IO () -- takes argument from command line
app args = do
     b <- buildPhoneBook
     forkIO $ evalArgs args b
     pure ()

buildPhoneBook :: IO PBState
buildPhoneBook = do
    s <- new
    sequence_ [ insert s ("Name" <> n) n | n <- maxNmbs ]
    pure s

evalArgs :: [String] -> PBState -> IO ()
evalArgs [] _         = putStrLn "Phone book quiting"
evalArgs xs phoneBook = forM_ xs $ \ x -> do
        mR <- lookup phoneBook $ Tx.pack x
        case mR of
           Nothing -> putStrLn "Lookup for unkown name!"
           Just p  -> printf "Found phone: %s for: %s \n" p x


