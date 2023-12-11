module Main (main) where

import Data (Coordinate, FColor (..), FieldObject (..), TileType (..), World (..))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Lib (initWorld, move)
import Static (down, fieldSize, fps, height, left, margin, right, scaleBy, up, width, windowPosition)

imagesPath :: [FilePath]
imagesPath =
  -- Order is hard coded and should NOT CHANGE (when changed => mapColor and mapType should be modified)
  [ "assets/crate_red_50x50.bmp",
    "assets/crate_blue_50x50.bmp",
    "assets/crate_green_50x50.bmp",
    "assets/storage_red_50x50.bmp",
    "assets/storage_blue_50x50.bmp",
    "assets/storage_green_50x50.bmp",
    "assets/paint_red_50x50.bmp",
    "assets/paint_blue_50x50.bmp",
    "assets/paint_green_50x50.bmp",
    "assets/robot_red_50x50.bmp",
    "assets/robot_blue_50x50.bmp",
    "assets/robot_green_50x50.bmp",
    "assets/wall_50x50.bmp"
  ]

mapColor :: FColor -> Int
mapColor Red = 0
mapColor Blue = 1
mapColor Green = 2
mapColor None = 0

mapType :: TileType -> Int
mapType Empty = 0
mapType Wall = 1
mapType Normal = 2

convert :: Coordinate -> (Float, Float)
convert (x, y) =
  ( fromIntegral $ x * fieldSize - width `div` 2 * fieldSize,
    fromIntegral $ (-y) * fieldSize + height `div` 2 * fieldSize
  )

construct :: [Picture] -> FieldObject -> Picture
construct images (FieldObject coord _ftype _fcolor) = translate x y $ images !! ((mapType _ftype * 3) + mapColor _fcolor)
  where
    (x, y) = convert coord

renderFields :: [Picture] -> World -> Picture
renderFields images world = Pictures $ (construct images <$> objects world) ++ [construct images $ head $ objects world] -- not correct

renderBackground :: Picture
renderBackground = Color (greyN 0.9) $ rectangleSolid (fromIntegral $ width * fieldSize) (fromIntegral $ height * fieldSize)

-- Zet het wereld om naar een Gloss picture.
render :: [Picture] -> World -> Picture
render images world = scale scaleBy scaleBy $ Pictures [renderBackground, renderFields images world]

handleInput :: Event -> World -> World
handleInput event world
  | isKey KeyRight event = move world right
  | isKey KeyUp event = move world up
  | isKey KeyLeft event = move world left
  | isKey KeyDown event = move world down
  | otherwise = world

window :: Display
window =
  InWindow
    "Colorban"
    ( round (fromIntegral (width * fieldSize + 2 * margin) * scaleBy),
      round (fromIntegral (height * fieldSize + 2 * margin) * scaleBy)
    )
    windowPosition

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _ _ = False

step :: Float -> World -> World
step _ b = b

main :: IO ()
main = do
  images <- mapM loadBMP imagesPath
  play window (greyN 0.1) fps initWorld (render images) handleInput step
