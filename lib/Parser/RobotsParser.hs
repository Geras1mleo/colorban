module Parser.RobotsParser (parseRobots) where

import Data (Robot (..))
import Parser.MyParser (Parser, capitalized, parseArray, parseFields1, parseName, readDouble)

getRobot :: Robot
getRobot =
  Robot
    { rname = undefined,
      rcoordinate = undefined,
      rcolor = undefined,
      selected = undefined,
      strength = undefined
    }

setRobotField :: Robot -> (String, String) -> Robot
setRobotField robot ("position", value) = robot {rcoordinate = read ("(" ++ value ++ ")")}
setRobotField robot ("color", value) = robot {rcolor = read value}
setRobotField robot ("selected", value) = robot {selected = read $ capitalized value}
setRobotField robot ("strength", value) = robot {strength = readDouble value}
setRobotField _ (key, _) = error ("Undefined robot key: \"" ++ key ++ "\"")

parseRobot :: Parser Robot
parseRobot = do
  name <- parseName
  let robot = getRobot {rname = name}
  foldl setRobotField robot <$> parseFields1

parseRobots :: Parser [Robot]
parseRobots = parseArray "robots" parseRobot
