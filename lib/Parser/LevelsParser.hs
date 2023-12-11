module Parser.LevelsParser (main) where

import Data
import Parser.LayoutParser (parseLayout)
import Parser.MyParser
import Parser.RobotsParser

testParse :: Parser (Layout, [Robot])
testParse = do
  layout <- parseLayout
  robots <- parseRobots
  return (layout, robots)

main :: IO ()
main = do
  result <- parseWithLeftOver testParse "./levels/example2.txt"
  case result of
    Left err -> print err
    Right xs -> print xs
