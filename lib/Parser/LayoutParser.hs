module Parser.LayoutParser (parseLayout) where

import Data (Layout (..), Tile (..), TileType, Dimentions)
import Parser.MyParser (Parser, char, many1, string, try, void, whitespace, anyChar)

getDimentions :: [[a]] -> Dimentions
getDimentions arr = (maximum (map length arr), length arr)

parseLayoutChar :: Parser TileType
parseLayoutChar = do
  void $ char ' '
  f <- anyChar
  return $ read [f]

parseLayoutLine :: Parser [TileType]
parseLayoutLine = whitespace >> void (char '|') >> many1 parseLayoutChar

parseLayout :: Parser Layout
parseLayout = do
  whitespace
  void $ string "- layout"
  field_types <- many1 (try parseLayoutLine)
  let fields_zipped = zip [0 ..] (map (zip [0 ..]) field_types)
  let tiles' =
        [ Tile {tcoordinate = (x, y), ttype = t}
          | (y, line) <- fields_zipped,
            (x, t) <- line
        ]
  return Layout {tiles = tiles', dimentions = getDimentions field_types}
