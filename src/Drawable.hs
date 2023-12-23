{-# LANGUAGE InstanceSigs #-}

module Drawable (Drawable (..), Textures) where

import BoardObject (BoardObject (..))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import GameData (Button, Coin (value), Crate (weight), Door (isOpened), IntF (..), Platform, Robot (selected, strength), Spot (durability), Storage, Tile (Tile), TileType (Empty, TileDown, TileLeft, TileRight, TileUp, Wall))
import Graphics.Gloss.Data.Picture (Picture (Pictures, Scale, Text, Translate))
import Static (assetsFolder)

type Textures = [(FilePath, Picture)]

dropPointZero :: String -> String
dropPointZero num = if mantissa == ".0" then intPart else num
  where
    (intPart, mantissa) = break (== '.') num

showNum :: (Show a) => a -> String
showNum = dropPointZero . show

showDouble :: (RealFloat a, Show a) => a -> String
showDouble d
  | isInfinite d = "~"
  | otherwise = showNum d

showIntF :: IntF -> String
showIntF Infinity = "~"
showIntF (Natural i) = showNum i

createCaption :: String -> Picture
createCaption text' = Scale 0.09 0.09 $ Text text'

createCaptionRightCorner :: Picture -> String -> Picture
createCaptionRightCorner pic str = Pictures [pic, Translate 10 (-20) $ createCaption str]

createCaptionCenter :: Picture -> String -> Picture
createCaptionCenter pic str = Pictures [pic, Translate (-3) (-4) $ createCaption str]

addSelectedBorder :: Bool -> Textures -> Picture -> Picture
addSelectedBorder selected' textures pic
  | not selected' = pic
  | otherwise = Pictures [pic, border]
  where
    maybeBorder = lookup (assetsFolder ++ "active_50x50.bmp") textures
    border = fromMaybe (error ("Selected robot border not found: " ++ show textures)) maybeBorder

class (BoardObject a) => Drawable a where
  getImagePath :: a -> String
  getImagePath obj = typeName ++ "s/" ++ typeName ++ "_" ++ colorName ++ "_50x50.bmp"
    where
      typeName = map toLower (tail $ show $ otype obj)
      colorName = map toLower (show $ color obj)
  draw :: a -> Textures -> Picture -> Picture
  draw _ _ p = p

instance Drawable Robot where
  draw :: Robot -> Textures -> Picture -> Picture
  draw robot textures pic = addSelectedBorder (selected robot) textures $ createCaptionRightCorner pic $ showDouble $ strength robot

instance Drawable Tile where
  getImagePath :: Tile -> String
  getImagePath (Tile Wall _) = "tiles/wall_50x50.bmp"
  getImagePath (Tile Empty _) = "tiles/wall_50x50.bmp"
  getImagePath (Tile TileLeft _) = "tiles/tile_left_50x50.bmp"
  getImagePath (Tile TileRight _) = "tiles/tile_right_50x50.bmp"
  getImagePath (Tile TileUp _) = "tiles/tile_up_50x50.bmp"
  getImagePath (Tile TileDown _) = "tiles/tile_down_50x50.bmp"
  getImagePath _ = "empty_50x50.bmp"

instance Drawable Storage

instance Drawable Crate where
  draw :: Crate -> Textures -> Picture -> Picture
  draw crate _ pic = createCaptionRightCorner pic $ showDouble $ weight crate

instance Drawable Spot where
  draw :: Spot -> Textures -> Picture -> Picture
  draw spot _ pic = createCaptionCenter pic $ showIntF $ durability spot

instance Drawable Door where
  getImagePath :: Door -> String
  getImagePath door
    | isOpened door = "objects/door_opened_50x50.bmp"
    | otherwise = "objects/door_locked_50x50.bmp"

instance Drawable Button where
  getImagePath :: Button -> String
  getImagePath _ = "objects/button_50x50.bmp"

instance Drawable Platform where
  getImagePath :: Platform -> String
  getImagePath _ = "objects/platform_50x50.bmp" -- TODO

instance Drawable Coin where
  getImagePath :: Coin -> String
  -- getImagePath _ = "objects/coin_50x50.bmp"
  getImagePath _ = "objects/coin_2_50x50.bmp"
  draw :: Coin -> Textures -> Picture -> Picture
  draw coin _ pic = createCaptionCenter pic $ show $ value coin
