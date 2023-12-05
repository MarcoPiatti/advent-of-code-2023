{-# LANGUAGE PatternGuards #-}

module Day01 ( day01 ) where

import Data.Char ( isDigit )
import Data.List ( find, isPrefixOf )

dict :: [String]
dict = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

toDigit :: String -> Char
toDigit "zero" = '0'
toDigit "one" = '1'
toDigit "two" = '2'
toDigit "three" = '3'
toDigit "four" = '4'
toDigit "five" = '5'
toDigit "six" = '6'
toDigit "seven" = '7'
toDigit "eight" = '8'
toDigit "nine" = '9'
toDigit _ = undefined

wordsAndDigits :: String -> String
wordsAndDigits = go []
    where
        go :: [Char] -> String -> [Char]
        go found "" = reverse found
        go found input@(x:xs)
            | isDigit x = go (x : found) xs
            | Just word <- find (`isPrefixOf` input) dict = go (toDigit word : found) xs
            | otherwise = go found xs 

digits :: (String -> String)
digits = filter isDigit

firstAndLast :: [a] -> [a]
firstAndLast elems = map ($ elems) [head, last]

parse :: (String -> String) -> String -> Int
parse preprocess =  read . firstAndLast . preprocess

day01 :: IO ()
day01 = do
    input <- lines <$> readFile "./input/day01.txt"
    putStr "Part 1: "
    print . sum . map (parse digits) $ input
    putStr "Part 2: "
    print . sum . map (parse wordsAndDigits) $ input