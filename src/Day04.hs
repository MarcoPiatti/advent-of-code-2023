{-# LANGUAGE TupleSections #-}

module Day04 ( day04 ) where

import Parser ( Parser, parse, symbol, intP, linesP )
import Text.Megaparsec ( some )
import Control.Monad ( void )
import Utils ( printResults )
import Data.List ( intersect )
import Data.Tuple.Extra ( first )

data Card = Card 
  { gameId :: Int
  , winner :: [Int]
  , owned :: [Int]
  } deriving (Show, Eq)

{-- Part 1 --}
ownedWinners :: Card -> Int
ownedWinners (Card _ win own) = length $ intersect win own

calcPoints :: Int -> Int
calcPoints 0 = 0
calcPoints matches = 2 ^ (matches - 1)

points :: Card -> Int
points = calcPoints . ownedWinners

part1 :: [Card] -> String
part1 = show . sum . map points
{-- Part 1 --}

{-- Part 2 --}
updateCopies :: (Int,Card) -> [(Int, Card)] -> [(Int, Card)]
updateCopies (amount, card) = uncurry (++) . first (map $ first (+ amount)) . splitAt (ownedWinners card)

copies :: [Card] -> [(Int, Card)] 
copies = countCopies . map (1,)
  where 
    countCopies :: [(Int, Card)] -> [(Int, Card)]
    countCopies [] = []
    countCopies (x:xs) = x : countCopies (updateCopies x xs)

part2 :: [Card] -> String
part2 = show . sum . map fst . copies
{-- Part 2 --}

{-- Parsing --}
cardP :: Parser Card  
cardP = (Card <$ symbol "Card") <*> intP <* void (symbol ":") <*> some intP <* symbol "|" <*> some intP

cardsP :: Parser [Card]
cardsP = linesP cardP
{-- Parsing --}

day04 :: IO ()
day04 = do
  input <- parse cardsP <$> readFile "./input/day04.txt"
  printResults [part1, part2] input