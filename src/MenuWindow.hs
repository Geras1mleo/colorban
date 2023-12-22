{-# LANGUAGE TupleSections #-}

module MenuWindow (render, handleInput, step) where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe)
import GameData (GameData (levels, playingLevel, windowType), Level (levelName), WindowType (GameWindow))
import Graphics.Gloss.Interface.IO.Interact (Event (EventKey), Key (Char), KeyState (Down), Picture (Color, Pictures, Scale, Text, Translate), rectangleSolid, scale, translate, white)
import LevelRenderer (Textures)
import Helper ((!?))

boldText :: String -> Picture
boldText text =
  Translate 0 0 $ Pictures $ map (\(offX, offY) -> Translate offX offY $ Text text) offsets
  where
    offsets = map (,0) [-3 .. 3] ++ map (0,) [-3 .. 3]

background :: Picture
background = Color white $ rectangleSolid 600 400

renderLevelButton :: Picture
renderLevelButton = translate 0 50 $ Scale 0.3 0.3 $ boldText "Select level:"

levelsButtons :: GameData -> [Picture]
levelsButtons gdata = shift 0 $ map (scale 0.3 0.3 . boldText . levelName') levels'
  where
    levels' = zip [1 :: Int ..] (levels gdata)
    levelName' (index, level) = show index ++ ": " ++ levelName level

buttonHeight :: Float
buttonHeight = 50

shift :: Float -> [Picture] -> [Picture]
shift _ [] = []
shift indent (x : xs) = Translate 0 (indent * buttonHeight) x : shift (indent - 1) xs

render :: Textures -> GameData -> Picture
render _ mData = Pictures [background, translate (-150) 100 $ Pictures (renderLevelButton : levelsButtons mData)]

handleInput :: Event -> GameData -> GameData
handleInput event gdata = fromMaybe gdata $ do
  num <- getNumKey event
  level <- levels gdata !? (num - 1)
  return gdata {windowType = GameWindow, playingLevel = Just level}

getNumKey :: Event -> Maybe Int
getNumKey (EventKey (Char k2) Down _ _)
  | isDigit k2 = Just $ digitToInt k2
getNumKey _ = Nothing

step :: Float -> GameData -> GameData
step _ l = l