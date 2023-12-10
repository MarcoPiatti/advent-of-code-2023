module Main (main) where

import System.IO ( hSetBuffering, stdin, stdout, BufferMode(NoBuffering) )
import Day01 ( day01 )
import Day02 ( day02 )
import Day03 ( day03 )
import Day04 ( day04 )
import Day05 ( day05 )

days :: [ IO () ]
days = [ day01, day02, day03, day04, day05 ]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr "Insert Desired Day: "
    dayNumber <- read <$> getLine
    days !! ( dayNumber - 1 )