{-# LANGUAGE InstanceSigs #-}

module Data
  ( Direction,
    Coordinate,
    FColor (..),
    BoardObject (..),
    TileType (..),
    Robot (..),
    Tile (..),
    FieldObject (..),
    Layout (..),
    World (..),
  )
where

import Data.Char (isAlpha)

class BoardObject b where
  coordinate :: b -> Coordinate

type Direction = (Int, Int)

type Coordinate = (Int, Int)

data FColor
  = Blue
  | Green
  | Red
  | Orange
  | Gray
  | Purple
  | None
  deriving (Eq, Show)

instance Read FColor where
  readsPrec :: Int -> ReadS FColor
  readsPrec _ str =
    let (str', rest) = span isAlpha str
     in [ ( case str' of
              "blue" -> Blue
              "green" -> Green
              "red" -> Red
              "orange" -> Orange
              "gray" -> Gray
              "purple" -> Purple
              "none" -> None
              _ -> error ("Undefined color: " ++ str'),
            rest
          )
        ]

data TileType
  = Normal
  | Wall
  | Empty
  | TileLeft
  | TileRight
  | TileUp
  | TileDown
  deriving (Eq, Show)

instance Read TileType where
  readsPrec :: Int -> ReadS TileType
  readsPrec _ str =
    let ch = head str
     in [ ( case ch of
              '.' -> Normal
              '#' -> Wall
              ' ' -> Empty
              '<' -> TileLeft
              '>' -> TileRight
              '^' -> TileUp
              'v' -> TileDown
              _ -> error ("Undefined tile type: " ++ [ch]),
            tail str
          )
        ]

data Robot = Robot
  { rname :: String,
    rcoordinate :: Coordinate,
    rcolor :: FColor,
    selected :: Bool,
    strength :: Double
  }
  deriving (Eq, Show)

instance BoardObject Robot where
  coordinate :: Robot -> Coordinate
  coordinate = rcoordinate

data Tile = Tile
  { ttype :: TileType,
    tcoordinate :: Coordinate
  }
  deriving (Eq, Show)

instance BoardObject Tile where
  coordinate :: Tile -> Coordinate
  coordinate = tcoordinate

data Storage = Storage
  { sname :: String,
    scoordinate :: Coordinate,
    scolor :: FColor
  }
  deriving (Eq, Show)

instance BoardObject Storage where
  coordinate :: Storage -> Coordinate
  coordinate = scoordinate

data Crate = Crate
  { cname :: String,
    ccoordinate :: Coordinate,
    ccolor :: FColor,
    weight :: Double
  }
  deriving (Eq, Show)

instance BoardObject Crate where
  coordinate :: Crate -> Coordinate
  coordinate = ccoordinate

data Spot = Spot
  { pname :: String,
    pcoordinate :: Coordinate,
    pcolor :: FColor,
    durability :: Int
  }
  deriving (Eq, Show)

instance BoardObject Spot where
  coordinate :: Spot -> Coordinate
  coordinate = pcoordinate

data Layout = Layout
  { tiles :: [Tile],
    dimentions :: (Int, Int)
  }
  deriving (Eq, Show)

data FieldObject = FieldObject
  { fcoordinate :: Coordinate,
    ftype :: TileType,
    fcolor :: FColor
  }
  deriving (Eq, Show)

data World = World
  { objects :: [FieldObject],
    robot :: Robot
  }
  deriving (Show)