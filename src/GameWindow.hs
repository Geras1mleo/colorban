module GameWindow (render, handleInput, step) where

import Data.List (find)
import Data.Maybe (fromJust)
import Game (isSolved, move, selectNextRobot, selectPreviousRobot)
import GameData (Direction, GameData (..), Level (levelName), WindowType (EndGameWindow, MenuWindow), getPlayingLevel)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), KeyState (Down), SpecialKey (..))
import LevelRenderer (Textures, renderLevel)
import Static (down, left, right, up)

render :: Textures -> GameData -> Picture
render textures = renderLevel textures . getPlayingLevel

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
      | isKey' 'n' event = setLevel $ selectNextRobot level
      | isKey' 'p' event = setLevel $ selectPreviousRobot level
      | isKey' 'r' event = setLevel $ resetLevel gdata
      | isKey' 'm' event = goToMenu gdata
      | otherwise = gdata

move' :: GameData -> Direction -> GameData
move' gdata dir = setLevel'
  where
    newLevel = move (getPlayingLevel gdata) dir
    solved = isSolved newLevel
    setLevel'
      | solved = gdata {windowType = EndGameWindow, playingLevel = Just newLevel}
      | otherwise = gdata {playingLevel = Just newLevel}

resetLevel :: GameData -> Level
resetLevel gdata = fromJust $ do
  thisLevel <- playingLevel gdata
  find (\l -> levelName l == levelName thisLevel) $ levels gdata

goToMenu :: GameData -> GameData
goToMenu gdata = gdata {windowType = MenuWindow}

isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _ _ = False

isKey' :: Char -> Event -> Bool
isKey' k1 (EventKey (Char k2) Down _ _) = k1 == k2
isKey' _ _ = False

step :: Float -> GameData -> GameData
step _ l = l
