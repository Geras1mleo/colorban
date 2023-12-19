module Parser.SpotsParser (parseSpots) where

import Data (Spot (..))
import Parser.MyParser (Parser, parseArray, parseFields1, parseName)

getSpot :: Spot
getSpot =
  Spot
    { spname = undefined,
      spcoordinate = undefined,
      spcolor = undefined,
      durability = undefined
    }

setSpotField :: Spot -> (String, String) -> Spot
setSpotField spot ("position", value) = spot {spcoordinate = read ("(" ++ value ++ ")")}
setSpotField spot ("color", value) = spot {spcolor = read value}
setSpotField spot ("durability", value) = spot {durability = read value}
setSpotField _ (key, _) = error ("Undefined Spot key: \"" ++ key ++ "\"")

parseSpot :: Parser Spot
parseSpot = do
  name <- parseName
  let spot = getSpot {spname = name}
  foldl setSpotField spot <$> parseFields1

parseSpots :: Parser [Spot]
parseSpots = parseArray "spots" parseSpot
