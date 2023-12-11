module Lib (initWorld, move, isSolved) where

import Data
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import Static (height, width)

initWorld :: World
initWorld =
  World
    { objects =
        [ FieldObject {fcoordinate = (2, 1), ftype = Wall, fcolor = Blue},
          FieldObject {fcoordinate = (5, 1), ftype = Wall, fcolor = Blue},
          FieldObject {fcoordinate = (2, 2), ftype = Wall, fcolor = Blue},
          FieldObject {fcoordinate = (1, 4), ftype = Wall, fcolor = Green},
          FieldObject {fcoordinate = (3, 4), ftype = Wall, fcolor = Red},
          FieldObject {fcoordinate = (4, 4), ftype = Wall, fcolor = Green},
          FieldObject {fcoordinate = (5, 4), ftype = Wall, fcolor = Green},
          FieldObject {fcoordinate = (3, 5), ftype = Wall, fcolor = Red},
          FieldObject {fcoordinate = (0, 0), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (1, 0), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (2, 0), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (3, 0), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (4, 0), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (5, 0), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (6, 0), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (0, 6), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (1, 6), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (2, 6), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (3, 6), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (4, 6), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (5, 6), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (6, 6), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (0, 1), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (0, 2), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (0, 3), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (0, 4), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (0, 5), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (6, 1), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (6, 2), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (6, 3), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (6, 4), ftype = Wall, fcolor = None},
          FieldObject {fcoordinate = (6, 5), ftype = Wall, fcolor = None}
        ],
      robot =
        Robot
          { rcoordinate = (1, 1),
            rcolor = Red,
            selected = False,
            strength = 0.5
          }
    }

newCoordinate :: Coordinate -> Direction -> Coordinate
newCoordinate (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

newCoordinate' :: (BoardObject a) => a -> Direction -> Coordinate
newCoordinate' a = newCoordinate (coordinate a)

inBounds :: Coordinate -> Bool
inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

findObject :: World -> Coordinate -> TileType -> Maybe FieldObject
findObject (World objs _) coord ftype_ = Nothing
  -- find (\field -> coordinate field == coord && ftype field == ftype_) objs

move :: World -> Direction -> World
move world direction
  | isSolved world = world
  | not $ canMove world direction = world
  | otherwise = changeColor $ moveObject (moveRobot world direction) direction

objectInBounds :: FieldObject -> Direction -> Bool
objectInBounds object direction = True
  -- inBounds (newCoordinate' object direction)

-- Checks if the object can move on the next field -> inbounds && next field is not a Crate and not a Wall
objectCanMove :: World -> FieldObject -> Direction -> Bool
objectCanMove world object direction = False
  -- objectInBounds object direction
  --   && isNothing (findObject world newCoord Crate)
  --   && isNothing (findObject world newCoord Wall)
  -- where
  --   newCoord = newCoordinate' object direction

-- Check if Robot can move and the Crate (!) ahead can move (if exist)
canMove :: World -> Direction -> Bool
canMove world direction = False
  -- objectInBounds (robot world) direction
  --   && isNothing (findObject world newCoord Wall)
  --   && Just False
  --     /= ( findObject world newCoord Crate
  --            >>= \crate ->
  --              return $
  --                fcolor crate == fcolor (robot world)
  --                  && objectCanMove world crate direction
  --        )
  -- where
  --   newCoord = newCoordinate' (robot world) direction

moveRobot :: World -> Direction -> World
moveRobot world direction = world
  -- World
  --   { objects = objects world,
  --     robot =
  --       FieldObject
  --         { coordinate = newCoordinate' (robot world) direction,
  --           ftype = Robot,
  --           fcolor = fcolor $ robot world
  --         }
  --   }

moveObject :: World -> Direction -> World
moveObject world direction = world
  -- fromMaybe
  --   world
  --   ( findObject world (coordinate $ robot world) Crate
  --       >>= \crate ->
  --         return $
  --           World
  --             { objects =
  --                 filter (/= crate) (objects world)
  --                   ++ [ FieldObject
  --                          { coordinate = newCoordinate' crate direction,
  --                            ftype = ftype crate,
  --                            fcolor = fcolor crate
  --                          }
  --                      ],
  --               robot = robot world
  --             }
  --   )

changeColor :: World -> World
changeColor world = world
  -- fromMaybe
  --   world
  --   ( findObject world (coordinate $ robot world) Paint
  --       >>= \paint ->
  --         return $
  --           World
  --             { objects = objects world,
  --               robot =
  --                 FieldObject
  --                   { coordinate = coordinate $ robot world,
  --                     ftype = ftype $ robot world,
  --                     fcolor = fcolor paint
  --                   }
  --             }
  --   )

isSolved :: World -> Bool
isSolved world = False
  -- all
  --   ( \crate ->
  --       Just True
  --         == ( findObject world (coordinate crate) Storage
  --                >>= \storage -> Just $ fcolor crate == fcolor storage
  --            )
  --   )
  --   crates
  -- where
  --   crates = filter (\field -> ftype field == Crate) (objects world)
