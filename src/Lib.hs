module Lib (
    Conf(..)
    , nextElem
    , getParam
    , fillConf
    , myGetOpts
    , tellConf
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
--  cnf   = rule       start lines     window move
--following :: (Eq a) => a -> [a] -> [a]
--following _ [] = []
--following x (y:l) = if x==y then l else following x l

nextElem :: String -> [String] -> String
nextElem _ [] = ""
nextElem str (x:xs) | x == str = head xs 
                    | otherwise = nextElem str xs

--nextElem e xs = listToMaybe . drop 1 . dropWhile (/= e) $ xs ++ (take 1 xs)

--nextElem :: Eq a => a -> [a] -> String
--nextElem _ [] = ""
--nextelem str list = "90"
--next :: (Eq a) => [a] -> a -> Maybe String
--next [] _ = Nothing
--next l x = if f == [] then Just(head l) else Just(head f)
--    where f = following x l:t

data Conf =  Conf { rule :: Maybe Int
                    , start :: Maybe Int
                    , linien :: Maybe Int
                    , window :: Maybe Int
                    , move :: Maybe Int
                    } deriving (Show)
--defaultConf :: Conf
--defaultConf = Conf {rule = Nothing, start = Nothing, linien = Nothing, window = Just 80, move = Nothing}

getParam :: String -> [String] -> Maybe Int
getParam  _ [] = Nothing 
getParam str (x:xs) = readMaybe $ nextElem str (x:xs)
    --readMaybe $ nextElem str (x:xs):: Maybe Int 
    --where res = readMaybe (nextElem str (x:xs)):: Maybe Int 
    --where res = readMaybe

fillConf :: [Maybe Int] -> Maybe Conf
fillConf [] = Nothing
fillConf (rule:start:linien:window:move:_) | rule == Nothing = Nothing
                                           | window > Just 80 = Nothing
                                           | (rule >= Just 0 && rule <= Just 255) = Just Conf {rule = rule, start = start, linien = linien, window = window, move = move}
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

tellConf :: Conf -> String
tellConf (Conf {rule = r, start = s, linien = l, window = w, move = m})
 = "this conf is:\nrule: " ++ show r ++ "\nstart: " ++ show s ++ "\nlinien: " ++
 show l ++"\nwindow :" ++ show w ++ "\nmove: " ++ show m  


toBin :: Int -> [Char]
toBin 0 = [' ']
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ ['*']
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [' ']
 
myWolfram :: Maybe Conf -> IO ()
myWolfram Nothing = goAway "The args were Falsch!" 84
myWolfram con = do
              putStrLn $ toBin 30
              goAway "" 0 
--myWolfram (Just con) = putStrLn $ tellConf con

goAway :: String -> Int -> IO ()
goAway x 84 = do
    putStrLn x
    exitWith (ExitFailure 84)
goAway x 0 = do
    exitWith(ExitSuccess)    


