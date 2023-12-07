module Parser 
( Parser
, ParseResult
, parse
, lexeme
, symbol
, wordP
, intP
, linesP
, some
, sepBy1
) where

import Data.Void ( Void )
import Text.Megaparsec ( Parsec, runParser, some, sepBy1, MonadParsec (takeWhile1P), sepEndBy1, errorBundlePretty )
import Text.Megaparsec.Char ( hspace1, eol )
import Text.Megaparsec.Error ( ParseErrorBundle )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAlpha)

type Parser = Parsec Void String
type ParseResult = Either (ParseErrorBundle String Void)

parse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
parse p = runParser p ""

sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

intP :: Parser Int
intP = lexeme L.decimal

wordP :: Parser String
wordP = lexeme $ takeWhile1P (Just "letter") isAlpha

linesP :: Parser a -> Parser [a]
linesP p = sepEndBy1 p eol