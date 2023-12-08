module Day03 ( day03 ) where

import Parser ( Parser, parse, lexeme, intP )
import Text.Megaparsec
    ( getSourcePos,
      satisfy,
      single,
      unPos,
      eitherP,
      many,
      sepEndBy,
      SourcePos(SourcePos) )
import Control.Applicative.Combinators ( (<|>) )
import Control.Monad ( void )
import Data.Char (isDigit)
import Utils ( printResults )
import Text.Megaparsec.Char (newline)
import Data.List ( foldl' )

data Position = Position 
  { span_x :: (Int, Int)
  , span_y :: (Int, Int)
  } deriving (Show, Eq)

data Number = Number 
  { number_val :: Int
  , number_pos :: Position
  } deriving (Show, Eq)

data Symbol = Symbol
  { symbol_char :: Char
  , symbol_pos :: Position
  } deriving (Show, Eq)

{-- Part 1 --}
isAdjacent :: Number -> Symbol -> Bool
isAdjacent num sym = adjacent (symbol_pos sym) (number_pos num) 
  where 
    adjacent 
      (Position (x1_start, x1_end) (y1_start, y1_end)) 
      (Position (x2_start, x2_end) (y2_start, y2_end))
      = x1_start >= x2_start - 1 && y1_start >= y2_start - 1 
      && x1_end <= x2_end + 1 && y1_end <= y2_end + 1

isValid :: [Symbol] -> Number -> Bool
isValid symbols number = any (isAdjacent number) symbols

part1 :: ([Number], [Symbol]) -> String
part1 (numbers, symbols) = show . sum . map number_val . filter (isValid symbols) $ numbers
{-- Part 1 --}

{-- Part 2 --}
adjacentNumbers :: [Number] -> Symbol -> [Number]
adjacentNumbers nums sym = filter (`isAdjacent` sym) nums 

gearRatios :: [[Number]] -> [Int]
gearRatios = map (product . map number_val) . filter ((==) 2 . length)

part2 :: ([Number], [Symbol]) -> String
part2 (numbers, symbols) = show . sum . gearRatios . map (adjacentNumbers numbers) $ symbols
{-- Part 2 --}

{-- Parsing --}
positionP :: Parser a -> Parser (Position, a)
positionP p = do 
  SourcePos _ rowStart colStart <- getSourcePos
  val <- p
  SourcePos _ rowEnd colEnd <- getSourcePos
  let pos = Position { span_x = (unPos colStart, unPos colEnd), span_y = (unPos rowStart, unPos rowEnd) }
  return (pos, val) 

numberP :: Parser Number
numberP = do 
  (pos, val) <- positionP intP
  return (Number val pos)

starP :: Parser Symbol
starP = do
  (pos,val) <- positionP . lexeme $ satisfy (\c -> c /= '.' && (not . isDigit) c)
  return (Symbol val pos)

ws :: Parser ()
ws = void . many $ single '.' <|> newline

elemsP :: Parser [Either Number Symbol]
elemsP = ws *> sepEndBy (eitherP numberP starP) ws

mapP :: Parser ([Number], [Symbol])
mapP = foldl' separate ([],[]) <$> elemsP
  where
    separate :: ([Number], [Symbol]) -> Either Number Symbol -> ([Number], [Symbol])
    separate (numbers, symbols) (Left num) = (num:numbers, symbols)
    separate (numbers, symbols) (Right sym) = (numbers, sym:symbols) 
{-- Parsing --}

day03 :: IO ()
day03 = do
  input <- parse mapP <$> readFile "./input/day03.txt"
  printResults [part1, part2] input