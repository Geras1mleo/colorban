module GameWindow (render, handleInput, step) where

import Control.Monad (guard)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust, fromMaybe)
import Game (isSolved, move, selectNextRobot, selectPreviousRobot)
import GameData (Direction, GameData (..), Level (levelName), WindowType (EndGameWindow, GameWindow, MenuWindow), getPlayingLevel)
import Graphics.Gloss (Picture (Blank, Pictures))
import Helper ((!?))
import LevelRenderer (renderLevel, renderSolvedCaption)
import Drawable (Textures)
import Static (down, left, right, up)
import RenderHelper ( isKey, isKey' )
import Graphics.Gloss.Interface.IO.Interact (SpecialKey(..))
import Graphics.Gloss.Interface.IO.Game (Event)

render :: Textures -> GameData -> Picture
render textures gdata = Pictures [renderLevel textures $ getPlayingLevel gdata, solvedPic]
  where
    solvedPic
      | playingLevelSolved gdata = renderSolvedCaption gdata
      | otherwise = Blank

handleInput :: Event -> GameData -> GameData
handleInput event gdata = updateLevel
  where
    level = getPlayingLevel gdata
    setLevel level' = gdata {playingLevel = Just level'}
    updateLevel
      | isKey KeyRight event = move' gdata right
      | isKey KeyUp event = move' gdata up
      | isKey KeyLeft event = move' gdata left
      | isKey KeyDown event = move' gdata down
      | isKey KeyEnter event = goToNextLevel gdata
      | isKey' 'n' event = setLevel $ selectNextRobot level
      | isKey' 'p' event = setLevel $ selectPreviousRobot level
      | isKey' 'r' event = resetLevel gdata
      | isKey' 'm' event = goToMenu gdata
      | otherwise = gdata

move' :: GameData -> Direction -> GameData
move' gdata dir =
  gdata
    { windowType = windowType',
      playingLevel = Just newLevel,
      playingLevelSolved = isLevelSolved
    }
  where
    newLevel = move (getPlayingLevel gdata) dir
    isLevelSolved = isSolved newLevel
    windowType' = fromMaybe GameWindow $ do
      guard isLevelSolved
      thisLevel <- playingLevel gdata
      index <- elemIndex thisLevel $ levels gdata
      guard $ index == (length (levels gdata) - 1)
      return EndGameWindow

resetLevel :: GameData -> GameData
resetLevel gdata = gdata {playingLevel = Just $ resetGetLevel gdata, playingLevelSolved = False}

resetGetLevel :: GameData -> Level
resetGetLevel gdata = fromJust $ do
  thisLevel <- playingLevel gdata
  find (\l -> levelName l == levelName thisLevel) $ levels gdata

goToMenu :: GameData -> GameData
goToMenu gdata = gdata {windowType = MenuWindow}

goToNextLevel :: GameData -> GameData
goToNextLevel gdata
  | playingLevelSolved gdata = gdata {playingLevel = Just nextLevel, playingLevelSolved = False}
  | otherwise = gdata
  where
    nextLevel = fromMaybe (getPlayingLevel gdata) $ do
      thisLevel <- playingLevel gdata
      index <- elemIndex thisLevel $ levels gdata
      levels gdata !? (index + 1)

step :: Float -> GameData -> GameData
step _ l = l
