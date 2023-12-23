module Static (left, right, up, down, margin, fps, windowPosition, fieldSize, scaleBy, thickLineOffsetsCount, assetsFolder, configFile) where

import GameData (Direction)

-- De mogelijke richtingen van de selector.
left, right, up, down :: Direction
up = (0, -1)
down = (0, 1)
right = (1, 0)
left = (-1, 0)

-- De ruimte langs de randen van het speelveld.
margin :: Int
margin = 0

scaleBy :: Float
scaleBy = 1.4

-- Framerate van het spel.
fps :: Int
fps = 60

-- Initiële positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (500, 100)

-- Voor implementatie van thickLine
thickLineOffsetsCount :: Integer
thickLineOffsetsCount = 5

-- De afmetingen van een tegel.
fieldSize :: Int
fieldSize = 50

assetsFolder :: String
assetsFolder = "./assets_2/"

configFile :: String
configFile = "levels/example.txt"