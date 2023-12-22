module Main (main) where

import Directory (getFilesRecursive)
import GameData (GameData (..), WindowType (..))
import GameWindow (handleInput, render, step)
import Graphics.Gloss (Display (FullScreen), Picture, loadBMP, play)
import Graphics.Gloss.Interface.IO.Game (Event)
import LevelRenderer (Textures, rgb)
import MenuWindow (handleInput, render, step)
import Parser.LevelsParser (parseLevels)
import Parser.MyParser (regularParse)
import Static (assetsFolder, configFile, fps)

render :: Textures -> GameData -> Picture
render textures gdata = handle $ windowType gdata
  where
    handle MenuWindow = MenuWindow.render textures gdata
    handle GameWindow = GameWindow.render textures gdata
    handle EndGameWindow = undefined -- TODO

handleInput :: Event -> GameData -> GameData
handleInput event gdata = handle $ windowType gdata
  where
    handle MenuWindow = MenuWindow.handleInput event gdata
    handle GameWindow = GameWindow.handleInput event gdata
    handle EndGameWindow = undefined -- TODO

step :: Float -> GameData -> GameData
step f gdata = handle $ windowType gdata
  where
    handle MenuWindow = MenuWindow.step f gdata
    handle GameWindow = GameWindow.step f gdata
    handle EndGameWindow = undefined -- TODO

main :: IO ()
main = do
  paths <- getFilesRecursive assetsFolder
  images <- mapM loadBMP paths
  gdata <- getGameData
  let textures = zip paths images
  play FullScreen (rgb (58, 58, 58)) fps gdata (Main.render textures) Main.handleInput Main.step

getGameData :: IO GameData
getGameData = do
  config <- readFile configFile
  let levels' = regularParse parseLevels config
  return $ case levels' of
    Left err -> error (show err)
    Right levels'' ->
      GameData
        { windowType = MenuWindow,
          levels = levels'',
          playingLevel = Nothing
        }