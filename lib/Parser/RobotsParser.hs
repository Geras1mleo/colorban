module Parser.RobotsParser (parseRobots) where

import Control.Monad (void)
import Data (Robot (..))
import Parser.MyParser
  ( capitalized,
    readDouble,
    parseArray,
    parseField,
    whitespace, trim,
  )
import Text.Parsec
  ( between,
    char,
    many1,
    noneOf,
    try,
  )
import Text.Parsec.String (Parser)

getRobot :: Robot
getRobot =
  Robot
    { rname = undefined,
      rcoordinate = undefined,
      rcolor = undefined,
      selected = undefined,
      strength = undefined
    }

parseRobotField :: Robot -> (String, String) -> Robot
parseRobotField robot ("position", value) = robot {rcoordinate = read ("(" ++ value ++ ")")}
parseRobotField robot ("color", value) = robot {rcolor = read value}
parseRobotField robot ("selected", value) = robot {selected = read $ capitalized value}
parseRobotField robot ("strength", value) = robot {strength = readDouble value}
parseRobotField _ (a, _) = error ("Undefined robot key: \"" ++ a ++ "\"")

parseRobot :: Parser Robot
parseRobot = do
  whitespace
  void $ char '|'
  name <- between (char ' ') (char '\n') (many1 (noneOf "\n"))
  fields <- many1 (try parseField)
  return $ foldl parseRobotField (getRobot {rname = trim name}) fields

parseRobots :: Parser [Robot]
parseRobots = parseArray "robots" parseRobot
