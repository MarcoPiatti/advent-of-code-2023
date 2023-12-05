module Main (main) where

import System.IO
import Day01 ( day01 )

days :: [ IO () ]
days = [ day01 ]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr "Insert Desired Day: "
    dayNumber <- read <$> getLine
    days !! ( dayNumber - 1 )