module Parser.MyParser (regularParse, parseWithLeftOver, whitespace, parseArray, readDouble, parseFields, trim, capitalized, char, noneOf, oneOf, string, anyToken, between, eof, many1, manyTill, many, parse, try, void, anyChar, ParseError, Parser, parseFields1, parseName, parseFieldArray1, parseTabsCount, count, parseTab, whitespaceTillNewLine, parseTabs) where

import Data.Char (isSpace, toLower, toUpper)
import Data.Functor (void, ($>))
import Data.List (dropWhileEnd)
import Text.Parsec (anyChar, anyToken, between, char, count, eof, many, many1, manyTill, noneOf, oneOf, parse, string, try)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>))
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

whitespaceTillNewLine :: Parser ()
whitespaceTillNewLine = void $ many $ try $ many (char ' ') >> char '\n'

parseTab :: Parser String
parseTab = string "    " $> "\t" <|> char '\t' $> "\t"

parseTabsCount :: Parser Int
parseTabsCount = many parseTab >>= \arr -> return $ length arr

parseTabs :: Int -> Parser [String]
parseTabs c = whitespaceTillNewLine >> count c parseTab

parseArray :: String -> Parser a -> Parser [a]
parseArray name elemParser =
  try
    ( do
        whitespaceTillNewLine
        tabsCount <- parseTabsCount
        void $ try (string ("- " ++ name))
        many
          ( try
              ( parseTabs (tabsCount + 1)
                  >> char '|'
                  >> elemParser
              )
          )
          <|> return []
    )
    <|> return []

parseFieldWithPrefix :: Char -> Parser (String, String)
parseFieldWithPrefix prefix = do
  whitespace
  void $ char prefix
  key <- between (char ' ') (char ' ') (many1 (noneOf " ")) -- TODO test with manyTill
  whitespace
  value <- between (char '(') (char ')') (many1 (noneOf ")"))
  return (trim key, trim value)

parseFields :: Parser [(String, String)]
parseFields = many $ try (parseFieldWithPrefix '*')

parseFields1 :: Parser [(String, String)]
parseFields1 = many1 $ try (parseFieldWithPrefix '*')

parseFieldArray1 :: Parser [(String, String)]
parseFieldArray1 = many1 $ try (parseFieldWithPrefix '|')

parseName :: Parser String
parseName = between (char ' ') (char '\n') (many1 (noneOf "\n")) >>= \s -> return (trim s)

readDouble :: String -> Double
readDouble "infinite" = 1 / 0
readDouble str = read str
