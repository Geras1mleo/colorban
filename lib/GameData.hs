{-# LANGUAGE InstanceSigs #-}

module GameData (Direction, Coordinate, Dimentions, FColor (..), TileType (..), Robot (..), Tile (..), Storage (..), Crate (..), Spot (..), Layout (..), IntF (..), isInfinity, Door (..), Button (..), Platform (..), Coin (..), Level (..), width, height, decrement, positive, GameData (..), WindowType (..), getPlayingLevel) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (fromMaybe)

type Direction = (Int, Int)

type Coordinate = (Int, Int)

type Dimentions = (Int, Int)

data IntF = Infinity | Natural Int
  deriving (Eq, Show)

isInfinity :: IntF -> Bool
isInfinity Infinity = True
isInfinity (Natural _) = False

readIntF :: String -> IntF
readIntF "infinite" = Infinity
readIntF str = Natural $ read str

decrement :: IntF -> IntF
decrement (Natural a) = Natural $ a - 1
decrement _ = Infinity

positive :: IntF -> Bool
positive (Natural a) = a > 0
positive _ = True

instance Read IntF where
  readsPrec :: Int -> ReadS IntF
  readsPrec _ str = let (str', rest) = span isAlphaNum str in [(readIntF str', rest)]

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

data Tile = Tile
  { ttype :: TileType,
    tcoordinate :: Coordinate
  }
  deriving (Eq, Show)

data Storage = Storage
  { sname :: String,
    scoordinate :: Coordinate,
    scolor :: FColor
  }
  deriving (Eq, Show)

data Crate = Crate
  { cname :: String,
    ccoordinate :: Coordinate,
    ccolor :: FColor,
    weight :: Double
  }
  deriving (Eq, Show)

data Spot = Spot
  { spname :: String,
    spcoordinate :: Coordinate,
    spcolor :: FColor,
    durability :: IntF
  }
  deriving (Eq, Show)

data Layout = Layout
  { tiles :: [Tile],
    dimentions :: Dimentions
  }
  deriving (Eq, Show)

data Button = Button
  { bcoordinate :: Coordinate,
    isPressed :: Bool
  }
  deriving (Eq, Show)

data Door = Door
  { dname :: String,
    doorCoordinate :: Coordinate,
    buttons :: [Button],
    isOpened :: Bool
  }
  deriving (Eq, Show)

data Platform = Platform
  { pname :: String,
    pdimentions :: Dimentions,
    startCoordinate :: Coordinate,
    endCoordinate :: Coordinate,
    probots :: [Robot],
    pcrates :: [Crate]
  }
  deriving (Eq, Show)

data Coin = Coin
  { coinName :: String,
    coinCoordinate :: Coordinate,
    value :: Int
  }
  deriving (Eq, Show)

data Level = Level
  { levelName :: String,
    layout :: Layout,
    robots :: [Robot],
    storages :: [Storage],
    crates :: [Crate],
    spots :: [Spot],
    doors :: [Door],
    platforms :: [Platform],
    coins :: [Coin],
    requiredCoins :: Int,
    collectedCoins :: Int
  }
  deriving (Show)

instance Eq Level where
  (==) :: Level -> Level -> Bool
  (==) a b = levelName a == levelName b
  (/=) :: Level -> Level -> Bool
  (/=) a b = not $ a == b


data WindowType = MenuWindow | GameWindow | EndGameWindow

data GameData = GameData
  { windowType :: WindowType,
    levels :: [Level],
    playingLevel :: Maybe Level,
    playingLevelSolved :: Bool
  }

getPlayingLevel :: GameData -> Level
getPlayingLevel = fromMaybe (error "Playing level hasn't been selected") . playingLevel

width :: Level -> Int
width l = fst $ dimentions $ layout l

height :: Level -> Int
height l = snd $ dimentions $ layout l