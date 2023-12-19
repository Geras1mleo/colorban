module Game (move, isSolved, getPlayedLevel) where

import BoardObject (BoardObject (..))
import Data
import Debug.Trace (trace)
import Parser.LevelsParser (parseLevels)
import Parser.MyParser (regularParse)
import Static (down, left, right, up)

replaceFirst :: (Eq a) => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst old new (x : xs)
  | x == old = new : xs
  | otherwise = x : replaceFirst old new xs

filter' :: (BoardObject a) => Coordinate -> [a] -> [a]
filter' coord = filter (\c -> coordinate c == coord)

getPlayedLevel :: String -> Level
getPlayedLevel input = do
  let levels = regularParse parseLevels input
  case levels of
    Left err -> error $ show err
    Right levels' -> head levels' -- TODO

newCoordinate :: (BoardObject a) => a -> Direction -> Coordinate
newCoordinate a (dx, dy) = (x + dx, y + dy)
  where
    (x, y) = coordinate a

newCoordinate' :: Coordinate -> Direction -> Coordinate
newCoordinate' (x, y) (dx, dy) = (x + dx, y + dy)

getSelectedRobot :: Level -> Robot
getSelectedRobot level = get $ filter selected $ robots level
  where
    get [x] = x
    get rs = error $ "Multiple robots selected: " ++ show rs

isAccessibleTileFrom :: Level -> Coordinate -> Direction -> Bool
isAccessibleTileFrom level coord direction = checkThis thisTiles && checkNext nextTiles
  where
    newCoord = newCoordinate' coord direction
    thisTiles = filter' coord $ tiles $ layout level
    nextTiles = filter' newCoord $ tiles $ layout level
    checkThis [] = True
    checkThis [Tile TileLeft _] = direction == left
    checkThis [Tile TileRight _] = direction == right
    checkThis [Tile TileUp _] = direction == up
    checkThis [Tile TileDown _] = direction == down
    checkThis [_] = True
    checkThis _ = error ("Multiple tiles on coordinate: " ++ show coord)
    checkNext [] = False
    checkNext [Tile Empty _] = False
    checkNext [Tile Wall _] = False
    checkNext [_] = True
    checkNext _ = error ("Multiple tiles on coordinate: " ++ show newCoord)

isAccessibleDoor :: Level -> Coordinate -> Bool
isAccessibleDoor level coord = check doors'
  where
    doors' = filter' coord $ doors level
    check [] = True
    check [d] = isOpened d
    check _ = error ("Multiple doors on coordinate: " ++ show coord)

canPush :: (BoardObject a) => Level -> a -> Double -> Direction -> Bool
canPush level obj restStrength direction =
  isAccessibleTileFrom level (coordinate obj) direction
    && isAccessibleDoor level newCoord
    && null nextRobots
    && checkCrates nextCrates
  where
    newCoord = newCoordinate obj direction
    nextCrates = filter' newCoord $ crates level
    nextRobots = filter' newCoord $ robots level
    checkCrates [] = True
    checkCrates [crate] =
      weight crate <= restStrength
        && color crate == color obj
        && canPush level crate (restStrength - weight crate) direction
    checkCrates _ = error $ "Multiple crates on position " ++ show (coordinate obj)

pushCrates :: [Crate] -> Coordinate -> Direction -> [Crate]
pushCrates crates' coord direction = pushCrate thisCrates'
  where
    thisCrates' = filter' coord crates'
    nextCoord = newCoordinate' coord direction
    new crate = crate {ccoordinate = nextCoord}
    pushCrate [] = crates'
    pushCrate [crate] = replaceFirst crate (new crate) (pushCrates crates' nextCoord direction)
    pushCrate _ = error ("Multiple crates on position " ++ show coord)

moveRobot :: Level -> Robot -> Coordinate -> Direction -> Level
moveRobot level old coord direction =
  level
    { robots = newRobots,
      crates = pushCrates (crates level) newCoord direction
    }
  where
    newCoord = newCoordinate' coord direction
    new = old {rcoordinate = newCoord}
    newRobots = replaceFirst old new $ robots level

handleSpots :: Level -> Level
handleSpots level =
  level
    { robots = newRobots spots',
      spots = newSpots spots'
    }
  where
    robot = getSelectedRobot level
    coord = coordinate robot
    spots' = filter (positive . durability) $ filter' coord $ spots level
    newRobots [] = robots level
    newRobots [spot] =
      let new = robot {rcolor = spcolor spot}
       in replaceFirst robot new $ robots level
    newRobots _ = error ("Multiple spots on coordinate: " ++ show coord)
    newSpots [] = spots level
    newSpots [spot] =
      let new = spot {durability = decrement $ durability spot}
       in replaceFirst spot new $ spots level
    newSpots _ = error ("Multiple spots on coordinate: " ++ show coord)

handleDoorsAndButtons :: Level -> Level
handleDoorsAndButtons level =
  level
    { doors = newDoors
    }
  where
    buttons' = concatMap buttons $ doors level
    pressedButtons = concat $ (filter'' <$> crates level) ++ (filter'' <$> robots level)
      where
        filter'' obj = filter' (coordinate obj) buttons'
    newButtons = map set
      where
        set button
          | button `elem` pressedButtons = button {isPressed = True}
          | otherwise = button {isPressed = False}
    newDoors = update <$> (set <$> doors level)
      where
        set door = door {buttons = newButtons $ buttons door}
        update door = door {isOpened = any isPressed (buttons door)}

handleCoins :: Level -> Level
handleCoins level =
  level
    { coins = newCoins,
      collectedCoins = collectedCoins'
    }
  where
    robotCoords = coordinate <$> robots level
    capturedCoins = concatMap (\coord -> filter' coord (coins level)) robotCoords
    newCoins = filter (`notElem` capturedCoins) $ coins level
    collectedCoins' = collectedCoins level + sum (value <$> capturedCoins)

move :: Level -> Direction -> Level
move level direction
  | canPush' =
      let level' = moveRobot level robot coord direction
       in let level'' = handleSpots level'
           in let level''' = handleDoorsAndButtons level''
              in let level'''' = handleCoins level'''
               in trace (show level'''') level''''
  | otherwise = level
  where
    robot = getSelectedRobot level
    coord = coordinate robot
    canPush' = canPush level robot (strength robot) direction

isSolved :: Level -> Bool
isSolved = undefined
