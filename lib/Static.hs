module Static (left, right, up, down, width, height, margin, fps, windowPosition, fieldSize, scaleBy) where

import Data (Direction)

-- De mogelijke richtingen van de selector.
left, right, up, down :: Direction
up = (0, -1)
down = (0, 1)
right = (1, 0)
left = (-1, 0)

-- Aantal tegels op de x-as.
width :: Int
width = 7

-- Aantal tegels op de y-as.
height :: Int
height = 7

-- De ruimte langs de randen van het speelveld.
margin :: Int
margin = 20

scaleBy :: Float
scaleBy = 1.25

-- Framerate van het spel.
fps :: Int
fps = 60

-- Initiële positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (500, 100)

-- De afmetingen van een tegel.
fieldSize :: Int
fieldSize = 50