module Day06 ( day06 ) where

import Parser ( Parser, parse, symbol, intP )
import Text.Megaparsec ( some )
import Text.Megaparsec.Char ( eol )
import Utils ( printResults )

data Race = Race
  { time :: Int
  , distance :: Int
  } deriving (Show, Eq)

{-- Part 1 --}
calculateDistance :: Int -> Int -> Int
calculateDistance maxTime chargeTime = chargeTime * (maxTime - chargeTime)

possibleOutcomes :: Race -> [Int]
possibleOutcomes (Race maxTime _) = map (calculateDistance maxTime) [0..maxTime]

winnerOutcomes :: Race -> [Int] -> Int
winnerOutcomes race = length . filter (> distance race)

part1 :: [Race] -> String
part1 = show . product . map (winnerOutcomes <*> possibleOutcomes)
{-- Part 1 --}

{-- Part 2 --}

-- chargeTime * (maxTime - chargeTime) = bestDistance
-- (-1) * x^2 + maxTime * x - bestDistance = 0
-- solve quadratic root with: root = ( (-maxTime) + sqrt (maxtime^2 - 4 * (-1) * (-bestDistance)) / 2 * (-1) )

firstWinner :: Race -> Int
firstWinner = ceiling . root
  where 
    root :: Race -> Double
    root (Race maxTime bestDistance) = (fromIntegral (-maxTime) + (sqrt . fromIntegral $ maxTime^(2::Int) - 4 * (-1) * (-bestDistance))) / 2 * (-1) 

part2 :: Race -> String
part2 race = show . (+1) . ((time race -) . (*2)) . firstWinner $ race
{-- Part 2 --}

{-- Parsing --}
timesP :: Parser [Int]
timesP = symbol "Time:" *> some intP

distancesP :: Parser [Int]
distancesP = symbol "Distance:" *> some intP

inputP :: Parser [Race]
inputP = zipWith Race <$> timesP <* eol <*> distancesP

input2P :: Parser Race
input2P = Race . read . concatMap show <$> timesP <* eol <*> (read . concatMap show <$> distancesP)
{-- Parsing --}

day06 :: IO ()
day06 = do
  input <- readFile "./input/Day06.txt"
  printResults [part1] $ parse inputP input 
  printResults [part2] $ parse input2P input
