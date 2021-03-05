module Main where

import Lib
import System.Environment
import Data.List
import Data.Maybe
import System.Exit
import Text.Read
import Data.Bits

main :: IO ()
main = do
    args <- getArgs
    --putStrLn "Hello, my type!"
    --putStrLn (args !! 1) it only works if there are at least 2 args!!
    myWolfram $ fillConf $ myGetOpts args
--main = someFunc
