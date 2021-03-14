module Lib (
    Conf(..)
    , Line (..)
    , nextElem
    , getParam
    , fillConf
    , myGetOpts
    , myWolfram
    , goAway

)
where

import System.Environment
import Data.List
import Data.Maybe
import System.Exit
import Text.Read
import Data.Bits

nextElem :: String -> [String] -> String
nextElem _ [] = ""
nextElem str (x:xs) | x == str = head xs 
                    | otherwise = nextElem str xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate x bla = bla : myReplicate (x-1) bla

myMod :: Int -> Int
myMod n| even n = n `div` 2 
       | otherwise = (n + 1 ) `div` 2

centerLine :: Int -> String -> Char -> String
centerLine win str pad| win >= (length str) =  rep ++ str ++ rest
        where rep = myReplicate (myMod (win - length str)) pad
              rest = myReplicate (win - length rep - length str) pad

centerLine _ [] _= []
centerLine  win  str _ = take win cutted
                    where cutted = drop (((length str) - win) `div` 2) str
 
 
getRealChar :: Bool -> [Char]
getRealChar True = "*"
getRealChar False = " "

ruleGetChar :: [Char] -> Int -> [Char]
ruleGetChar [' ', ' ', ' '] n = getRealChar $ testBit n 0
ruleGetChar [' ', ' ', '*'] n = getRealChar $ testBit n 1
ruleGetChar [' ', '*', ' '] n = getRealChar $ testBit n 2
ruleGetChar [' ', '*', '*'] n = getRealChar $ testBit n 3
ruleGetChar ['*', ' ', ' '] n = getRealChar $ testBit n 4
ruleGetChar ['*', ' ', '*'] n = getRealChar $ testBit n 5
ruleGetChar ['*', '*', ' '] n = getRealChar $ testBit n 6
ruleGetChar ['*', '*', '*'] n = getRealChar $ testBit n 7

data Line = Line { left :: [Char] 
              , check :: [Char]
             , right :: [Char] 
} deriving (Show)


getLeft :: Int -> [Char] -> Char -> [Char]
getLeft r (x:xs) y = ruleGetChar [f, s, t] r
                  where f = x
                        s = x
                        t = y    

getRight :: Int -> Char ->[Char] -> [Char]
getRight r y (x:xs) = ruleGetChar [f, s, t] r
                  where f = y
                        s = x 
                        t = x    

newGeneration :: Int -> String -> String
newGeneration r str | length (take 3 str) < 3 = ""
                    | otherwise = (++) (ruleGetChar seg r) (
                        newGeneration r (drop 1 str))
                            where seg = take 3 str   


myExpand :: Int -> String -> Line -> String
myExpand r str ln = res ++ (getRight r (
    last $ check ln) (right ln))
    where res = (++) (getLeft r (left ln) (head $ check ln)) str 
           
fLine::Line
fLine = Line {left = " ", check = "*", right = " "} 

genNextLine :: Line -> Maybe Int -> Line 
genNextLine ln (Just ru) = Line l c r
                where
                    l = left ln
                    r = left ln
                    c = myExpand ru (
                        newGeneration ru (lineToString ln)) ln

lineToString :: Line -> String
lineToString l = (++) ((++) (left l) (check l)) (right l)

data Conf =  Conf { ru :: Maybe Int
                    , st :: Maybe Int
                    , li :: Maybe Int
                    , wi :: Maybe Int
                    , mo :: Maybe Int
                    } deriving (Show)

getParam :: String -> [String] -> Maybe Int
getParam  _ [] = Nothing 
getParam str (x:xs) = readMaybe $ nextElem str (x:xs)

fillConf :: [Maybe Int] -> Maybe Conf
fillConf [] = Nothing
fillConf (r:s:l:w:m:_) | r == Nothing = Nothing
            | w > Just 80 = Nothing
 | (r >= Just 0 && r <= Just 255) = Just Conf {
     ru = r, st = s, li = l, wi = w, mo = m}
           | otherwise = Nothing

myGetOpts :: [String] -> [Maybe Int]
myGetOpts [] = []
myGetOpts args = [rule, start, linien, window, move]
    where
        rule = getParam "--rule" args
        start = getParam "--start" args    
        linien = getParam "--lines" args
        window = getParam "--window" args
        move = getParam "--move" args

myLoop:: Maybe Int -> Conf -> Line -> IO ()
myLoop Nothing con ln = 
   putStrLn (centerLine 80 (lineToString ln) (head $ left ln)) >>
   myLoop Nothing con (genNextLine ln (ru con))
myLoop (Just 0) con _ = putStr "" 
myLoop (Just n) con ln = 
    putStrLn (centerLine 80 (lineToString ln) (head $ left ln)) >> 
    myLoop (Just (n - 1)) con (genNextLine ln (ru con))    

myStart :: Maybe Int -> Maybe Int -> Line -> Line 
myStart (Just 0) (Just r) ln = ln
myStart Nothing _ ln = ln
myStart (Just n) (Just r) ln = myStart (
    Just (n - 1)) (Just r) (genNextLine ln (Just r))


myWolfram :: Maybe Conf -> IO ()
myWolfram Nothing = goAway "The args were Falsch!" 84
myWolfram (Just con) = myLoop (li con) con (
    myStart (st con) (ru con) fLine) >>
              goAway "" 0 

goAway :: String -> Int -> IO ()
goAway x 84 = 
    putStrLn x >>
    exitWith (ExitFailure 84)
goAway x 0 = 
    exitWith(ExitSuccess)    


