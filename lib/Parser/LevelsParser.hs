module Parser.LevelsParser (parseLevels) where

import Control.Monad (when)
import GameData
import Parser.CoinsParser (parseCoins)
import Parser.CratesParser (parseCrates)
import Parser.DoorsParser (parseDoors)
import Parser.LayoutParser (parseLayout)
import Parser.MyParser (Parser, anyChar, char, many, many1, parseFields, parseName, string, try, void, whitespace)
import Parser.PlatformsParser (parsePlatforms)
import Parser.RobotsParser (parseRobots)
import Parser.SpotsParser (parseSpots)
import Parser.StoragesParser (parseStorages)

getLevel :: Level
getLevel =
  Level
    { levelName = undefined,
      layout = undefined,
      robots = undefined,
      storages = undefined,
      crates = undefined,
      spots = undefined,
      doors = undefined,
      platforms = undefined,
      coins = undefined,
      requiredCoins = 0,
      collectedCoins = 0
    }

setLevelField :: Level -> (String, String) -> Level
setLevelField level ("required_coins", value') = level {requiredCoins = read value'}
setLevelField _ (key, _) = error ("Undefined Level key: \"" ++ key ++ "\"")

levelArraysParser :: Level -> Parser Level
levelArraysParser level = do
  layout' <- parseLayout
  robots' <- parseRobots
  storages' <- parseStorages
  crates' <- parseCrates
  spots' <- parseSpots
  doors' <- parseDoors
  platforms' <- parsePlatforms
  coins' <- parseCoins
  return
    level
      { layout = layout',
        robots = robots',
        storages = storages',
        crates = crates',
        spots = spots',
        doors = doors',
        platforms = platforms',
        coins = coins'
      }

parseLevel :: Parser Level
parseLevel = do
  name <- parseName
  level <- foldl setLevelField (getLevel {levelName = name}) <$> parseFields
  levelArraysParser level

handleUnparsedConfig :: Parser ()
handleUnparsedConfig = do
  whitespace
  rest <- try (many anyChar)
  when (rest /= "") $ error ("Unparsed config at the end of file: " ++ rest)

parseLevels :: Parser [Level]
parseLevels = do
  void $ string "levels"
  many1 parseArrayElement <* handleUnparsedConfig
  where
    parseArrayElement = try (whitespace >> char '|' >> parseLevel)
