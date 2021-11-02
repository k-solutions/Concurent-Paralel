module ParFibon where

import           Control.Monad.Par

countFibonNum :: Int -> Int
countFibonNum num | num <= 1  = 1
                  | otherwise = num1 + num2 + 1
                        where
                          (num1, num2) = runPar $ do
                                                     fx <- spawnP $ countFibonNum (num - 1)
                                                     gx <- spawnP $ countFibonNum (num - 2)
                                                     n1 <- get fx
                                                     n2 <- get gx
                                                     pure (n1, n2)
