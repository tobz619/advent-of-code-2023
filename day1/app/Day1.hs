{-#LANGUAGE OverloadedStrings #-}
module Day1 where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative hiding (many, some)
import Data.Void (Void)
import Data.List (tails, isPrefixOf)
import Data.Char (digitToInt, isDigit)


type Parser = Parsec Void String

answer :: IO ()
answer = do
    inp <- lines <$> readFile "text\\day1sample.txt"
    let ans1 = foldr ((+) . getFirstLast) 0 inp
    let ans2 = foldr ((+) . getFirstLast . concatMap replace . tails) 0 inp 
    putStrLn $ "Answer 1: " ++ show ans1
    putStrLn $ "Answer 2: " ++ show ans2


replace :: String -> String
replace [] = []
replace str@(x:xs)
 | "one" `isPrefixOf` str = '1': replace (drop 3 str)
 | "two" `isPrefixOf` str = '2': replace (drop 3 str)
 | "three" `isPrefixOf` str = '3': replace (drop 5 str)
 | "four" `isPrefixOf` str = '4': replace (drop 4 str)
 | "five" `isPrefixOf` str = '5': replace (drop 4 str)
 | "six" `isPrefixOf` str = '6': replace (drop 3 str)
 | "seven" `isPrefixOf` str = '7': replace (drop 5 str)
 | "eight" `isPrefixOf` str = '8': replace (drop 5 str)
 | "nine" `isPrefixOf` str = '9': replace (drop 4 str)
 | otherwise = x: replace xs

getFirstLast :: [Char] -> Int
getFirstLast xs = digitToInt (head filtered) * 10 + digitToInt (last filtered)
                where filtered = filter isDigit xs