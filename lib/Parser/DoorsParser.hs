module Parser.DoorsParser (parseDoors) where

import GameData(Door (..))
import Parser.MyParser (Parser, parseArray, parseFields1, parseName)
import Parser.ButtonsParser (parseButtons)

getDoor :: Door
getDoor =
  Door
    { dname = undefined,
      doorCoordinate = undefined,
      buttons = undefined,
      isOpened = False
    }

setDoorField :: Door -> (String, String) -> Door
setDoorField door ("position", value) = door {doorCoordinate = read ("(" ++ value ++ ")")}
setDoorField _ (key, _) = error ("Undefined Door key: \"" ++ key ++ "\"")

parseDoor :: Parser Door
parseDoor = do
  name <- parseName
  let door = getDoor {dname = name}
  door' <- foldl setDoorField door <$> parseFields1
  buttons' <- parseButtons
  return door' {buttons = buttons'}

parseDoors :: Parser [Door]
parseDoors = parseArray "doors" parseDoor
