module Parser.MyParser (regularParse, parseWithLeftOver, whitespace, parseArray, readDouble, parseField, trim, capitalized, char, noneOf, oneOf, string, anyToken, between, eof, many1, manyTill, many, parse, try, void, ParseError, Parser) where

import Control.Monad (void)
import Data.Char (isSpace, toLower, toUpper)
import Data.List (dropWhileEnd)
import Text.Parsec (ParseError, anyToken, between, char, eof, many, many1, manyTill, noneOf, oneOf, parse, string, try)
import Text.Parsec.String (Parser)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

capitalized :: String -> String
capitalized (head' : tail') = toUpper head' : map toLower tail'
capitalized [] = []

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithLeftOver :: Parser a -> FilePath -> IO (Either ParseError (a, String))
parseWithLeftOver p fname = do
  input <- readFile fname
  return (parse ((,) <$> p <*> leftOver) fname input)
  where
    leftOver = manyTill anyToken eof

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parseArray :: String -> Parser a -> Parser [a]
parseArray name elemParser = whitespace >> void (string ("- " ++ name)) >> many1 (try elemParser)

parseField :: Parser (String, String)
parseField = do
  whitespace
  void $ char '*'
  key <- between (char ' ') (char ' ') (many1 (noneOf " "))
  whitespace
  value <- between (char '(') (char ')') (many1 (noneOf ")"))
  return (trim key, trim value)

readDouble :: String -> Double
readDouble "infinite" = 1 / 0
readDouble str = read str