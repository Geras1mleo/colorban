module Main (main) where

import Data (Level (levelName), height, width)
import Game (move, selectNextRobot, selectPreviousRobot)
import Graphics.Gloss (Display (InWindow), Picture, greyN, loadBMP, play)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, MouseButton, SpecialKey), KeyState (Down), MouseButton (LeftButton), SpecialKey (..))
import LevelRenderer (renderLevel, rgb, Textures)
import Parser.LevelsParser (parseLevels)
import Parser.MyParser (regularParse)
import Static (down, fieldSize, fps, left, margin, right, scaleBy, up, windowPosition, assetsFolder)
import Directory (getFilesRecursive)

render :: Textures -> Level -> Picture
render = renderLevel

handleInput :: Event -> Level -> Level
handleInput event level
  | isKey KeyRight event = move level right
  | isKey KeyUp event = move level up
  | isKey KeyLeft event = move level left
  | isKey KeyDown event = move level down
  | isKey' 'n' event = selectNextRobot level
  | isKey' 'p' event = selectPreviousRobot level
  | otherwise = level

window :: Level -> Display
window l =
  InWindow
    ("Colorban -> " ++ levelName l)
    ( round (fromIntegral (width l * fieldSize + 2 * margin) * scaleBy),
      round (fromIntegral (height l * fieldSize + 2 * margin) * scaleBy)
    )
    windowPosition

isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey k1 (EventKey (MouseButton LeftButton) Down _ (x, y)) = False
isKey _ _ = False

isKey' :: Char -> Event -> Bool
isKey' k1 (EventKey (Char k2) Down _ _) = k1 == k2
isKey' _ _ = False

step :: Float -> Level -> Level
step _ l = l

main :: IO ()
main = do
  paths <- getFilesRecursive assetsFolder
  images <- mapM loadBMP paths
  let images' = zip paths images
  config <- readFile "levels/example2.txt"
  let levels = regularParse parseLevels config
  let level = case levels of
        Left err -> error $ show err
        Right levels' -> head levels'
  print level
  play (window level) (rgb (58, 58, 58)) fps level (render images') handleInput step
