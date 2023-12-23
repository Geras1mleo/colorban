{-# LANGUAGE TupleSections #-}

module RenderHelper (rgb, rgba, convertToCartesian, boldText, isKey, isKey', convertToCartesian') where

import GameData (Coordinate, Level, height, width)
import Graphics.Gloss (Picture (Pictures), makeColorI, Color)
import Graphics.Gloss.Interface.IO.Interact (Picture (Text, Translate), SpecialKey, Event (EventKey), Key (SpecialKey, Char), KeyState (Down))
import Static (fieldSize)

rgb :: (Int, Int, Int) -> Color
rgb (r, g, b) = rgba (r, g, b, 255)

rgba :: (Int, Int, Int, Int) -> Color
rgba (r, g, b, a) = makeColorI r g b a

convertToCartesian :: (Int, Int) -> Int -> Coordinate -> (Float, Float)
convertToCartesian (width', height') fieldSize' (x, y) =
  ( fromIntegral $ (x - (width' `div` 2)) * fieldSize' + xHalf,
    fromIntegral $ ((-y) + (height' `div` 2)) * fieldSize' - yHalf
  )
  where
    xHalf = ((width' + 1) `mod` 2) * (fieldSize' `div` 2)
    yHalf = ((height' + 1) `mod` 2) * (fieldSize `div` 2)

convertToCartesian' :: Level -> Coordinate -> (Float, Float)
convertToCartesian' level = convertToCartesian (width level, height level) fieldSize

boldText :: String -> Picture
boldText text =
  Translate 0 0 $ Pictures $ map (\(offX, offY) -> Translate offX offY $ Text text) offsets
  where
    offsets = map (,0) [-3 .. 3] ++ map (0,) [-3 .. 3]


isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _ _ = False

isKey' :: Char -> Event -> Bool
isKey' k1 (EventKey (Char k2) Down _ _) = k1 == k2
isKey' _ _ = False