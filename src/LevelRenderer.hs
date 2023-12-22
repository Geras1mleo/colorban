{-# LANGUAGE TupleSections #-}

module LevelRenderer (renderLevel, rgb, rgba, Textures) where

import BoardObject (BoardObject (coordinate))
import GameData(Button (isPressed), Coordinate, Door (buttons), Layout (tiles), Level (coins, crates, doors, layout, platforms, robots, spots, storages), height, width)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Drawable (Drawable (draw, getImagePath))
import Graphics.Gloss (Color, Picture (Color, Line, Pictures), Point, makeColorI, rectangleSolid, scale, translate)
import Static (fieldSize, scaleBy, thickLineOffsetsCount, assetsFolder)

type Textures = [(FilePath, Picture)]

rgb :: (Int, Int, Int) -> Color
rgb (r, g, b) = rgba (r, g, b, 255)

rgba :: (Int, Int, Int, Int) -> Color
rgba (r, g, b, a) = makeColorI r g b a

convertToCartesian :: Level -> Coordinate -> (Float, Float)
convertToCartesian l (x, y) =
  ( fromIntegral $ (x - (width l `div` 2)) * fieldSize + xHalf,
    fromIntegral $ ((-y) + (height l `div` 2)) * fieldSize - yHalf
  )
  where
    xHalf = ((width l + 1) `mod` 2) * (fieldSize `div` 2)
    yHalf = ((height l + 1) `mod` 2) * (fieldSize `div` 2)

getImage' :: (Drawable a) => Textures -> a -> Picture
getImage' textures obj =
  snd $
    fromMaybe (error ("Texture not found: " ++ path)) $
      find (\(path', _) -> path' == path) textures
  where
    path = assetsFolder ++ getImagePath obj

makeImage :: (Drawable a) => Textures -> Level -> a -> Picture
makeImage textures l boardObj = translate x y (draw boardObj image)
  where
    (x, y) = convertToCartesian l $ coordinate boardObj
    image = getImage' textures boardObj

-- Dark magic here
thickLine :: Float -> Point -> Point -> Picture
thickLine thickness p1 p2 =
  Pictures $ map (\offset -> Line [offsetPoint (offset / 2) p1, offsetPoint (offset / 2) p2]) offsets
  where
    offsets = map ((thickness *) . fromIntegral) [1 - thickLineOffsetsCount .. thickLineOffsetsCount - 1]
    offsetPoint offset (x, y) = (x + offset, y)

makeDoorButtonConnection :: Level -> [Picture]
makeDoorButtonConnection level = drawLine <$> coords
  where
    coords = concatMap (\door -> map (coordinate door,) (coordWithState door)) $ doors level
      where
        coordWithState door = (\b -> (coordinate b, isPressed b)) <$> buttons door
    drawLine (c1, (c2, isPressed')) = colorizeLine $ thickLine 1 p1 p2
      where
        p1 = convertToCartesian level c1
        p2 = convertToCartesian level c2
        colorizeLine p
          | isPressed' = Color (rgba (0, 128, 0, 60)) p
          | otherwise = Color (rgba (255, 0, 0, 60)) p

renderFields :: Textures -> Level -> Picture
renderFields textures l = do
  let tiles' = makeImage textures l <$> tiles (layout l)
  let robots' = makeImage textures l <$> robots l
  let storages' = makeImage textures l <$> storages l
  let crates' = makeImage textures l <$> crates l
  let spots' = makeImage textures l <$> spots l
  let doors' = makeImage textures l <$> doors l
  let buttons' = makeImage textures l <$> concatMap buttons (doors l)
  let platforms' = makeImage textures l <$> platforms l
  let coins' = makeImage textures l <$> coins l
  let lines' = makeDoorButtonConnection l
  Pictures (lines' ++ tiles' ++ coins' ++ storages' ++ spots' ++ doors' ++ buttons' ++ crates' ++ platforms' ++ robots')

renderBackground :: Level -> Picture
renderBackground level =
  Color (rgb (229, 229, 229)) $ rectangleSolid (fromIntegral $ width level * fieldSize) (fromIntegral $ height level * fieldSize)

renderLevel :: Textures -> Level -> Picture
renderLevel textures level = scale scaleBy scaleBy $ Pictures [renderBackground level, renderFields textures level]
