module Utils ( printResults ) where

import Parser ( ParseResult )
import Text.Megaparsec.Error ( errorBundlePretty )

printResults :: [a -> String] -> ParseResult a -> IO ()
printResults fs = either (putStrLn . errorBundlePretty) (mapM_ putStrLn . flip map fs . flip ($))