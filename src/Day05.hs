module Day05 ( day05 ) where

import Parser ( Parser, parse, symbol, intP, wordP, linesP )
import Control.Applicative ( asum )
import Text.Megaparsec ( some )
import Text.Megaparsec.Char ( eol )
import Utils ( printResults )
import Data.List ( foldl' )
import Data.Maybe (fromMaybe)

data RangeEntry = RangeEntry 
  { outStart :: Int
  , inStart :: Int
  , rangeSpan :: Int
  } deriving (Show, Eq)

type Range = (Int,Int)

{-- Part 1 --}
getOut :: Int -> RangeEntry-> Maybe Int
getOut x (RangeEntry out input range)
  | x >= input && x < input + range = Just (out + x - input) 
  | otherwise = Nothing

findOut :: Int -> [RangeEntry] -> Int
findOut input = fromMaybe input . asum . map (getOut input)

lastOut :: Int -> [[RangeEntry]] -> Int
lastOut = foldl' findOut 

part1 :: ([Int], [[RangeEntry]]) -> String
part1 (seeds, maps) = show . minimum $ map (`lastOut` maps) seeds
{-- Part 1 --}

{-- Part 2 --}
leftOverlap :: Range -> RangeEntry -> Bool
leftOverlap (start, inspan) (RangeEntry _ start2 inspan2) = start < start2 && start + inspan <= start2 + inspan2 && inspan > start2

rightOverlap :: Range -> RangeEntry -> Bool
rightOverlap (start, inspan) (RangeEntry _ start2 inspan2) = start + inspan > start2 + inspan2 && start >= start2 && start < start2 + inspan2

contains :: Range -> RangeEntry -> Bool
contains (start, inspan) (RangeEntry _ start2 inspan2) = start < start2 && start + inspan > start2 + inspan2

splitRange :: RangeEntry -> Range -> [Range]
splitRange re@(RangeEntry _ start2 inspan2) r@(start, inspan)
  | contains r re = [(start, start2 - start), (start2, inspan2), (start2 + inspan2, start + inspan - start2 - inspan2)]
  | leftOverlap r re = [(start, start2 - start) , (start2, start + inspan - start2)]
  | rightOverlap r re = [(start, start2 + inspan2 - start), (start2 + inspan2, start + inspan - start2 - inspan2)]
  | otherwise = [r]

splitRanges :: [Range] -> RangeEntry -> [Range]
splitRanges ranges entry = ranges >>= splitRange entry

splitRangesStep :: [Range] -> [RangeEntry] -> [Range]
splitRangesStep = foldl' splitRanges

getOutRange :: Range -> RangeEntry -> Maybe Range
getOutRange (start, inspan) (RangeEntry outstart2 start2 inspan2) 
  | start >= start2 && start + inspan <= start2 + inspan2 = Just (outstart2 + start - start2, inspan)
  | otherwise = Nothing

findOutRange :: Range -> [RangeEntry] -> Range
findOutRange input = fromMaybe input . asum . map (getOutRange input)

findOutRanges :: [RangeEntry] -> [Range] -> [Range]
findOutRanges aMap = map (`findOutRange` aMap)

step :: [RangeEntry] -> [Range] -> [Range]
step aMap = findOutRanges aMap . (`splitRangesStep` aMap)

lastOutRange :: [Range] -> [[RangeEntry]] -> [Range]
lastOutRange = foldl' (flip step) 

part2 :: ([Range], [[RangeEntry]]) -> String
part2 = show . minimum . map fst . uncurry lastOutRange
{-- Part 2 --}

{-- Parsing --}
seedsP :: Parser [Int]
seedsP = symbol "seeds:" *> some intP

rangeP :: Parser Range
rangeP = (,) <$> intP <*> intP

seedsRangesP :: Parser [Range]
seedsRangesP = symbol "seeds:" *> some rangeP

rangeEntryP :: Parser RangeEntry
rangeEntryP = RangeEntry <$> intP <*> intP <*> intP

mapP :: Parser [RangeEntry]
mapP = wordP *> symbol "-to-" *> wordP *> symbol "map:" *> eol *> linesP rangeEntryP

mapsP :: Parser [[RangeEntry]]
mapsP = linesP mapP

inputP :: Parser ([Int], [[RangeEntry]])
inputP = (,) <$> seedsP <* some eol <*> mapsP

input2P :: Parser ([Range], [[RangeEntry]])
input2P = (,) <$> seedsRangesP <* some eol <*> mapsP
{-- Parsing --}

day05 :: IO ()
day05 = do
  input <- readFile "./input/Day05.txt"
  printResults [part1] $ parse inputP input
  printResults [part2] $ parse input2P input
