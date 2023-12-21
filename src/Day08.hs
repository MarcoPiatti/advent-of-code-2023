module Day08 ( day08 ) where

import Parser ( Parser, parse, symbol, wordP, linesP )
import Text.Megaparsec ( some )
import Control.Applicative.Combinators ( (<|>) )
import Text.Megaparsec.Char ( eol )
import Utils ( printResults )
import qualified Data.Map.Strict as Map

type Instruction = Map.Map String Node -> Node -> Node

instruction :: (Node -> String) -> Instruction
instruction side nodes current = nodes Map.! side current

l :: Instruction
l = instruction nodeLeft

r :: Instruction
r = instruction nodeRight

data Node = Node 
  { nodeId :: String
  , nodeLeft :: String
  , nodeRight :: String
  } deriving (Show, Eq)

{-- Part 1 --}
sequenceLength :: (String -> Bool) -> [Instruction] -> Map.Map String Node -> (Int, Node) -> Int
sequenceLength _ [] _ (count, _) = count
sequenceLength finishCondition (i:is) nodes (count, node)
  | finishCondition . nodeId $ node = count
  | otherwise = sequenceLength finishCondition is nodes (count + 1, i nodes node)

part1 :: ([Instruction], Map.Map String Node) -> String
part1 (is, ns) = show . sequenceLength (== "ZZZ") (cycle is) ns $ (0, ns Map.! "AAA")
{-- Part 1 --}

{-- Part 2 --}
{-- Iterating for every combination is not sustainable --}
{-- We must find each individual sequence length and get total LCM --}
part2 :: ([Instruction], Map.Map String Node) -> String
part2 (is, ns) = show . foldr (lcm . sequenceLength ((==) 'Z' . last) instructions ns) 1 $ start
  where
    instructions :: [Instruction]
    instructions = cycle is
    start :: [(Int, Node)]
    start = map ((,) 0 . snd) . Map.toList . Map.filterWithKey (\k _ -> last k == 'A') $ ns
{-- Part 2 --}

{-- Parsing --}
instructionP :: Parser Instruction
instructionP = l <$ symbol "L" <|> r <$ symbol "R"

nodeP :: Parser Node
nodeP = Node <$> wordP <* symbol "=" <* symbol "(" <*> wordP <* symbol "," <*> wordP <* symbol ")"

inputP :: Parser ([Instruction],  Map.Map String Node)
inputP = (,) <$> some instructionP <* eol <* eol <*> (Map.fromList . map (\n -> (nodeId n, n)) <$> linesP nodeP)
{-- Parsing --}

day08 :: IO ()
day08 = do
  input <- readFile "./input/day08.txt"
  printResults [part1, part2] $ parse inputP input