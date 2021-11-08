module WindowManager where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Map               (Map, (!))
import           Data.Set               (Set)
import qualified Data.Set               as S

type Desktop = Int
type Window = Int
type UserFocus = TVar Desktop
type Display = Map Desktop (TVar (Set Window))

--- API ---

moveWindow ::  Display
            -> Window
            -> Desktop
            -> Desktop
            -> IO ()
moveWindow disp win a b = atomically $ moveWindowSTM disp win a b

moveWindowSTM ::  Display
               -> Window
               -> Desktop
               -> Desktop
               -> STM ()
moveWindowSTM disp win a b = do
    wa <- readTVar ma
    wb <- readTVar mb
    writeTVar ma (S.delete win wa)
    writeTVar mb (S.insert win wb)
  where
    ma = disp ! a
    mb = disp ! b

render :: Set Window -> IO ()
render = undefined

swapWindows ::  Display
             -> Window -> Desktop
             -> Window -> Desktop
             -> IO ()
swapWindows disp w a v b = atomically $ do
  moveWindowsSTM disp w a b
  moveWindowsSTM disp v b a

moveWindowsSTM = undefined

getWindows :: Display -> UserFocus -> STM (Set Window)
getWindows disp focus = do
    desktop <- readTVar focus
    readTVar (disp ! desktop)

renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus = do
    wins <- atomically $ getWindows disp focus
    loop wins
  where
    loop wins = do
      render wins
      next <- atomically $ do
                wins' <- getWindows disp focus
                if wins == wins'
                  then retry
                  else pure wins'
      loop next
