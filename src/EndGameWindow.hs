module EndGameWindow (render, handleInput, step) where

import Drawable (Textures)
import GameData (GameData (windowType), WindowType (MenuWindow))
import Graphics.Gloss (Picture, rectangleSolid, scale, translate)
import Graphics.Gloss.Interface.IO.Interact (Event, Picture (Color, Pictures), SpecialKey (..), white)
import RenderHelper (boldText, isKey, isKey')

background :: Picture
background = Color white $ rectangleSolid 1100 150

renderCaption :: Picture
renderCaption = translate (-400) 0 $ scale 0.3 0.3 $ boldText "You won! Press enter to go back to menu..."

render :: Textures -> GameData -> Picture
render _ _ = Pictures [background, renderCaption]

handleInput :: Event -> GameData -> GameData
handleInput event gdata
  | isKey KeyEnter event = gdata {windowType = MenuWindow}
  | isKey' 'm' event = gdata {windowType = MenuWindow}
  | otherwise = gdata

step :: Float -> GameData -> GameData
step _ l = l