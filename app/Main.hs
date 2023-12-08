module Main (main) where

import System.IO
import Day01 ( day01 )
import Day02 ( day02 )
import Day03 ( day03 )

days :: [ IO () ]
days = [ day01, day02, day03 ]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr "Insert Desired Day: "
    dayNumber <- read <$> getLine
    days !! ( dayNumber - 1 )