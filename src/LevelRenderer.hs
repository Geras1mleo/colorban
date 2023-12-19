{-# LANGUAGE TupleSections #-}

module LevelRenderer (renderLevel) where

import Assets (getImageIndex)
import BoardObject (BoardObject (coordinate))
import Data (Button (isPressed), Coordinate, Door (buttons), Layout (tiles), Level (coins, crates, doors, layout, platforms, robots, spots, storages), height, width)
import Drawable (Drawable (draw))
import Graphics.Gloss (Color, Picture (Color, Line, Pictures), Point, greyN, makeColorI, rectangleSolid, scale, translate)
import Static (fieldSize, scaleBy)

convertToCartesian :: Level -> Coordinate -> (Float, Float)
convertToCartesian l (x, y) =
  ( fromIntegral $ (x - (width l `div` 2)) * fieldSize + xHalf,
    fromIntegral $ ((-y) + (height l `div` 2)) * fieldSize - yHalf
  )
  where
    xHalf = ((width l + 1) `mod` 2) * (fieldSize `div` 2)
    yHalf = ((height l + 1) `mod` 2) * (fieldSize `div` 2)

getImage' :: (Drawable a) => [Picture] -> a -> Picture
getImage' images obj = images !! getImageIndex obj

makeImage :: (Drawable a) => [Picture] -> Level -> a -> Picture
makeImage images l boardObj = translate x y (draw boardObj image)
  where
    (x, y) = convertToCartesian l $ coordinate boardObj
    image = getImage' images boardObj

rgb :: (Int, Int, Int) -> Color
rgb (r, g, b) = makeColorI r g b 60

thickLine :: Float -> Point -> Point -> Picture
thickLine thickness p1 p2 =
  Pictures $ map (\offset -> Line [offsetPoint (offset / 2) p1, offsetPoint (offset / 2) p2]) offsets
  where
    numOffsets = 5
    offsets = map ((thickness *) . fromIntegral) [1 - numOffsets .. numOffsets - 1]
    offsetPoint offset (x, y) = (x + offset, y)

makeDoorButtonConnection :: Level -> [Picture]
makeDoorButtonConnection level = drawLine <$> coords
  where
    coords = concatMap (\door -> map (coordinate door,) (coordWithState door)) $ doors level
      where
        coordWithState door = (\b -> (coordinate b, isPressed b)) <$> buttons door
    drawLine (c1, (c2, colorize)) = colorizeLine $ thickLine 1 p1 p2
      where
        p1 = convertToCartesian level c1
        p2 = convertToCartesian level c2
        colorizeLine p
          | colorize = Color (rgb (0, 128, 0)) p
          | otherwise = Color (rgb (255, 0, 0)) p

renderFields :: [Picture] -> Level -> Picture
renderFields images l = do
  let tiles' = makeImage images l <$> tiles (layout l)
  let robots' = makeImage images l <$> robots l
  let storages' = makeImage images l <$> storages l
  let crates' = makeImage images l <$> crates l
  let spots' = makeImage images l <$> spots l
  let doors' = makeImage images l <$> doors l
  let buttons' = makeImage images l <$> concatMap buttons (doors l)
  let platforms' = makeImage images l <$> platforms l
  let coins' = makeImage images l <$> coins l
  let lines' = makeDoorButtonConnection l
  Pictures (lines' ++ tiles' ++ coins' ++ storages' ++ spots' ++ doors' ++ buttons' ++ crates' ++ platforms' ++ robots')

renderBackground :: Level -> Picture
renderBackground level =
  Color (greyN 0.9) $ rectangleSolid (fromIntegral $ width level * fieldSize) (fromIntegral $ height level * fieldSize)

renderLevel :: [Picture] -> Level -> Picture
renderLevel images level = scale scaleBy scaleBy $ Pictures [renderBackground level, renderFields images level]
