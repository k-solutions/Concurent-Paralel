module Main where

import           ParFibon

main = do
    putStr "Enter Number"
    strN <- getLine
    let res = countFibonNum (read strN) :: Int
    print res
