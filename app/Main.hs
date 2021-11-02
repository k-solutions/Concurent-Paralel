module Main where

import           System.Environment

import           PhoneBook

main = getArgs >>= app >> getLine
