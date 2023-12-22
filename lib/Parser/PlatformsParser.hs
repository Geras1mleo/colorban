module Parser.PlatformsParser (parsePlatforms) where

import GameData(Platform (..))
import Parser.MyParser (Parser, parseArray, parseFields1, parseName)

getPlatform :: Platform
getPlatform =
  Platform
    { pname = undefined,
      pdimentions = undefined,
      startCoordinate = undefined,
      endCoordinate = undefined,
      probots = [],
      pcrates = []
    }

setPlatformField :: Platform -> (String, String) -> Platform
setPlatformField platform ("dimentions", value) = platform {pdimentions = read ("(" ++ value ++ ")")}
setPlatformField platform ("start_position", value) = platform {startCoordinate = read ("(" ++ value ++ ")")}
setPlatformField platform ("end_position", value) = platform {endCoordinate = read ("(" ++ value ++ ")")}
setPlatformField _ (key, _) = error ("Undefined Platform key: \"" ++ key ++ "\"")

parsePlatform :: Parser Platform
parsePlatform = do
  name <- parseName
  let platform = getPlatform {pname = name}
  foldl setPlatformField platform <$> parseFields1

parsePlatforms :: Parser [Platform]
parsePlatforms = parseArray "platforms" parsePlatform
