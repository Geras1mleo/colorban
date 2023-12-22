{-# LANGUAGE InstanceSigs #-}

module BoardObject (BoardObject (..)) where

import GameData(Button (bcoordinate), Coin (coinCoordinate), Coordinate, Crate (ccolor, ccoordinate), Door (doorCoordinate), FColor (None), Platform (startCoordinate), Robot (rcolor, rcoordinate), Spot (spcolor, spcoordinate), Storage (scolor, scoordinate), Tile (tcoordinate))

data ObjectType
  = TRobot
  | TCrate
  | TStorage
  | TSpot
  | TTile
  | TDoor
  | TButton
  | TPlatform
  | TCoin
  deriving (Eq, Show)

class BoardObject b where
  coordinate :: b -> Coordinate
  otype :: b -> ObjectType
  color :: b -> FColor
  color _ = None

-- TODO orientation?

instance BoardObject Robot where
  coordinate :: Robot -> Coordinate
  coordinate = rcoordinate
  otype :: Robot -> ObjectType
  otype _ = TRobot
  color :: Robot -> FColor
  color = rcolor

instance BoardObject Tile where
  coordinate :: Tile -> Coordinate
  coordinate = tcoordinate
  otype :: Tile -> ObjectType
  otype _ = TTile

instance BoardObject Storage where
  coordinate :: Storage -> Coordinate
  coordinate = scoordinate
  otype :: Storage -> ObjectType
  otype _ = TStorage
  color :: Storage -> FColor
  color = scolor

instance BoardObject Crate where
  coordinate :: Crate -> Coordinate
  coordinate = ccoordinate
  otype :: Crate -> ObjectType
  otype _ = TCrate
  color :: Crate -> FColor
  color = ccolor

instance BoardObject Spot where
  coordinate :: Spot -> Coordinate
  coordinate = spcoordinate
  otype :: Spot -> ObjectType
  otype _ = TSpot
  color :: Spot -> FColor
  color = spcolor

instance BoardObject Door where
  coordinate :: Door -> Coordinate
  coordinate = doorCoordinate
  otype :: Door -> ObjectType
  otype _ = TDoor

instance BoardObject Button where
  coordinate :: Button -> Coordinate
  coordinate = bcoordinate
  otype :: Button -> ObjectType
  otype _ = TButton

instance BoardObject Platform where
  coordinate :: Platform -> Coordinate
  coordinate = startCoordinate
  otype :: Platform -> ObjectType
  otype _ = TPlatform

instance BoardObject Coin where
  coordinate :: Coin -> Coordinate
  coordinate = coinCoordinate
  otype :: Coin -> ObjectType
  otype _ = TCoin