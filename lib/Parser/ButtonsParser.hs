module Parser.ButtonsParser (parseButtons) where

import Control.Monad (void)
import GameData(Button (..))
import Parser.MyParser (Parser, parseFieldArray1, whitespace, string, try)

getButton :: Button
getButton =
  Button
    { bcoordinate = undefined,
      isPressed = False
    }

getButtonElem :: (String, String) -> Button
getButtonElem ("position", value) = getButton {bcoordinate = read ("(" ++ value ++ ")")}
getButtonElem (key, _) = error ("Undefined Button key: \"" ++ key ++ "\"")

parseButtons :: Parser [Button]
parseButtons = try $ do
  whitespace
  void $ string "- buttons"
  whitespace
  map getButtonElem <$> parseFieldArray1
