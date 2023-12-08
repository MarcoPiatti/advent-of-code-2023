module Day02 ( day02 ) where

import Parser
import Text.Megaparsec ( sepBy1 )
import Data.Function ( on )
import Data.List ( foldl' )
import Control.Applicative ( Applicative(liftA2) )
import Utils ( printResults )

data Cube = Cube 
  { quantity :: Int
  , color :: String
  } deriving (Show, Eq)

type Set = [Cube]

data Game = Game
  { gameId :: Int
  , sets :: [Set]
  } deriving (Show, Eq)

{-- Part 1 --}
elfBag :: [Cube]
elfBag = [Cube 12 "red", Cube 13 "green", Cube 14 "blue"]

exceeds :: Cube -> Cube -> Bool
exceeds a b = on (==) color a b && on (<) quantity a b

setExceeds :: Set -> Set -> Bool
setExceeds bag = or . liftA2 exceeds bag

isPossible :: Set -> Game -> Bool
isPossible bag = not . any (setExceeds bag) . sets

possibleIDs :: Set -> [Game] -> [Int]
possibleIDs bag = map gameId . filter (isPossible bag)

part1 :: [Game] -> String
part1 = show . sum . possibleIDs [Cube 12 "red", Cube 13 "green", Cube 14 "blue"]
{-- Part 1 --}

{-- Part 2 --}
necessarySet :: Set -> Cube -> Set
necessarySet [] cube = [cube]
necessarySet (x:xs) cube
  | on (==) color x cube = if on (<) quantity x cube then cube:xs else x:xs
  | otherwise = x : necessarySet xs cube

minimumSet :: Game -> Set
minimumSet = foldl' necessarySet [] . concat . sets

powerOfMinimums :: [Game] -> [Int]
powerOfMinimums = map (product . map quantity . minimumSet)

part2 :: [Game] -> String
part2 = show . sum . powerOfMinimums
{-- Part 2 --}

{-- Parsing --}
cubeP :: Parser Cube
cubeP = Cube <$> intP <*> wordP

setP :: Parser Set
setP = sepBy1 cubeP (symbol ",")

gameP :: Parser Game
gameP = symbol "Game" *> (Game <$> intP) <* symbol ":" <*> sepBy1 setP (symbol ";")

gamesP :: Parser [Game]
gamesP = linesP gameP
{-- Parsing --}

day02 :: IO ()
day02 = do
  input <- parse gamesP <$> readFile "./input/day02.txt"
  printResults [part1, part2] input