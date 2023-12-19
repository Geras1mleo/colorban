module Main (main) where

import Assets (imagesPath)
import Data
import LevelRenderer (renderLevel)
import Game (getPlayedLevel, move)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (MouseButton, SpecialKey), KeyState (Down), MouseButton (LeftButton), SpecialKey (..))
import Static (down, fieldSize, fps, left, margin, right, scaleBy, up, windowPosition)

render :: [Picture] -> Level -> Picture
render = renderLevel

handleInput :: Event -> Level -> Level
handleInput event level
  | isKey KeyRight event = move level right
  | isKey KeyUp event = move level up
  | isKey KeyLeft event = move level left
  | isKey KeyDown event = move level down
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
isKey k1 (EventKey (SpecialKey k2) Down _ (x, y)) = {-trace ("x: " ++ show x ++ "y: " ++ show y) $-} k1 == k2
isKey k1 (EventKey (MouseButton LeftButton) Down _ (x, y)) = {-trace ("x: " ++ show x ++ "y: " ++ show y)-} True
isKey _ _ = False

step :: Float -> Level -> Level
step _ l = l

main :: IO ()
main = do
  images <- mapM loadBMP imagesPath
  config <- readFile "levels/example.txt"
  let level = getPlayedLevel config
  print level
  play (window level) (greyN 0.1) fps level (render images) handleInput step
