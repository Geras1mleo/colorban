module MenuWindow (render, handleInput, step) where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe)
import GameData (GameData (levels, playingLevel, playingLevelSolved, windowType), Level (levelName), WindowType (GameWindow))
import Graphics.Gloss.Interface.IO.Interact (Event (EventKey), Key (Char), KeyState (Down), Picture (Color, Pictures, Scale), rectangleSolid, scale, translate, white)
import Helper ((!?))
import Drawable (Textures)
import RenderHelper (boldText, convertToCartesian)

background :: GameData -> Picture
background gdata = Color white $ rectangleSolid 1200 height'
  where height' = fromIntegral $ buttonHeight * (length (levels gdata) + 2)

renderLevelButton :: GameData -> Picture
renderLevelButton gdata = translate x y $ Scale 0.3 0.3 $ boldText "Select level:"
  where
    (x,y) = convertToCartesian (10, length (levels gdata)) buttonHeight (0, 0)

levelsButtons :: GameData -> [Picture]
levelsButtons gdata = map (\(y, pic) -> translate 0 (cart y) pic) pictures'
  where
    cart y = snd $ convertToCartesian (10, length (levels gdata)) buttonHeight (0, y)
    levels' = zip [1 :: Int ..] (levels gdata)
    levelName' (index, level) = show index ++ ": " ++ levelName level
    pictures = map (scale 0.3 0.3 . boldText . levelName') levels'
    pictures' = zip [1 :: Int ..] pictures

buttonHeight :: Int
buttonHeight = 50

render :: Textures -> GameData -> Picture
render _ gdata = Pictures [background gdata, translate (-100) 0 $ Pictures (renderLevelButton gdata : levelsButtons gdata)]

handleInput :: Event -> GameData -> GameData
handleInput event gdata = fromMaybe gdata $ do
  num <- getNumKey event
  level <- levels gdata !? (num - 1)
  return gdata {windowType = GameWindow, playingLevel = Just level, playingLevelSolved = False}

getNumKey :: Event -> Maybe Int
getNumKey (EventKey (Char k2) Down _ _)
  | isDigit k2 = Just $ digitToInt k2
getNumKey _ = Nothing

step :: Float -> GameData -> GameData
step _ l = l