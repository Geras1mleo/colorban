module Main (main) where

import Directory (getFilesRecursive)
import GameData (GameData (..), WindowType (..))
import GameWindow (handleInput, render, step)
import Graphics.Gloss (Display (FullScreen), Picture, loadBMP, play)
import Graphics.Gloss.Interface.IO.Game (Event)
import Drawable (Textures)
import MenuWindow (handleInput, render, step)
import EndGameWindow (handleInput, render, step)
import Parser.LevelsParser (parseLevels)
import Parser.MyParser (regularParse)
import RenderHelper (rgb)
import Static (assetsFolder, configFile, fps)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

render :: Textures -> GameData -> Picture
render textures gdata = handle $ windowType gdata
  where
    handle MenuWindow = MenuWindow.render textures gdata
    handle GameWindow = GameWindow.render textures gdata
    handle EndGameWindow = EndGameWindow.render textures gdata

handleInput :: Event -> GameData -> GameData
handleInput event gdata = handle $ windowType gdata
  where
    handle MenuWindow = MenuWindow.handleInput event gdata
    handle GameWindow = GameWindow.handleInput event gdata
    handle EndGameWindow = EndGameWindow.handleInput event gdata

step :: Float -> GameData -> GameData
step f gdata = handle $ windowType gdata
  where
    handle MenuWindow = MenuWindow.step f gdata
    handle GameWindow = GameWindow.step f gdata
    handle EndGameWindow = EndGameWindow.step f gdata

main :: IO ()
main = do
  paths <- getFilesRecursive assetsFolder
  images <- mapM loadBMP paths
  gdata <- runMaybeT getGameData
  let textures = zip paths images
  case gdata of
    Nothing -> error ("Error: Could not load configuration " ++ configFile)
    Just gdata' -> play FullScreen (rgb (58, 58, 58)) fps gdata' (Main.render textures) Main.handleInput Main.step

readConfigFile :: FilePath -> MaybeT IO String
readConfigFile filePath = MaybeT $ do
  content <- liftIO $ readFile filePath
  return $ Just content

getGameData :: MaybeT IO GameData
getGameData = do
  config <- readConfigFile configFile
  let levels' = regularParse parseLevels config
  return $ case levels' of
    Left err -> error (show err)
    Right levels'' ->
      GameData
        { windowType = MenuWindow,
          levels = levels'',
          playingLevel = Nothing,
          playingLevelSolved = False
        }