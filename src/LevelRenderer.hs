{-# LANGUAGE TupleSections #-}

module LevelRenderer (renderLevel, renderSolvedCaption) where

import BoardObject (BoardObject (coordinate))
import Data.Maybe (fromMaybe)
import Drawable (Drawable (draw, getImagePath), Textures)
import GameData (Button (isPressed), Door (buttons), GameData, Layout (tiles), Level (coins, collectedCoins, crates, doors, layout, platforms, requiredCoins, robots, spots, storages), getPlayingLevel, height, width)
import Graphics.Gloss (Picture, Point, rectangleSolid, scale, translate, yellow)
import Graphics.Gloss.Data.Picture (Picture (..))
import RenderHelper (boldText, convertToCartesian', rgb, rgba)
import Static (assetsFolder, fieldSize, scaleBy, thickLineOffsetsCount)

getImage' :: (Drawable a) => Textures -> a -> Picture
getImage' textures obj = image
  where
    path = assetsFolder ++ getImagePath obj
    maybeImage = lookup path textures
    image = fromMaybe (error ("Texture not found: " ++ path)) maybeImage

makeImage :: (Drawable a) => Textures -> Level -> a -> Picture
makeImage textures level boardObj = translate x y (draw boardObj textures image)
  where
    (x, y) = convertToCartesian' level $ coordinate boardObj
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
        p1 = convertToCartesian' level c1
        p2 = convertToCartesian' level c2
        colorizeLine p
          | isPressed' = Color (rgba (0, 128, 0, 60)) p
          | otherwise = Color (rgba (255, 0, 0, 60)) p

renderFields :: Textures -> Level -> Picture
renderFields textures level = do
  let tiles' = makeImage' <$> tiles (layout level)
  let robots' = makeImage' <$> robots level
  let storages' = makeImage' <$> storages level
  let crates' = makeImage' <$> crates level
  let spots' = makeImage' <$> spots level
  let doors' = makeImage' <$> doors level
  let buttons' = makeImage' <$> concatMap buttons (doors level)
  let platforms' = makeImage' <$> platforms level
  let coins' = makeImage' <$> coins level
  let lines' = makeDoorButtonConnection level
  Pictures (lines' ++ tiles' ++ coins' ++ storages' ++ spots' ++ doors' ++ buttons' ++ crates' ++ platforms' ++ robots')
  where
    makeImage' :: (Drawable a) => a -> Picture
    makeImage' = makeImage textures level

renderBackground :: Level -> Picture
renderBackground level = Color (rgb (229, 229, 229)) $ rectangleSolid width' height'
  where
    width' = fromIntegral $ width level * fieldSize
    height' = fromIntegral $ height level * fieldSize

renderHeader :: Textures -> Level -> Picture
renderHeader _ level = Color yellow $ Pictures [requiredCoinsPic', collectedCoinsPic']
  where
    (x, y) = convertToCartesian' level (1, 0)
    scale' = scale 0.15 0.15
    requiredCoinsPic = boldText ("Required coins: " ++ show (requiredCoins level))
    collectedCoinsPic = boldText ("Collected coins: " ++ show (collectedCoins level))
    requiredCoinsPic' = translate x y (scale' requiredCoinsPic)
    collectedCoinsPic' = translate x (y - 20) (scale' collectedCoinsPic)

renderSolvedCaption :: GameData -> Picture
renderSolvedCaption gdata = Color (rgb (0, 228, 255)) $ translate x y (scale' pic)
  where
    level = getPlayingLevel gdata
    scale' = scale 0.2 0.2
    (x, y) = convertToCartesian' level (0, height level + 1)
    pic = boldText "Level solved! Press enter to go to next level..."

renderLevel :: Textures -> Level -> Picture
renderLevel textures level = scale scaleBy scaleBy $ Pictures [renderBackground level, renderFields textures level, renderHeader textures level]
