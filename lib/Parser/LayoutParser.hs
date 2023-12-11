module Parser.LayoutParser (parseLayout) where

import Data (Layout (..), Tile (..), TileType)
import Parser.MyParser (Parser, char, many1, oneOf, string, try, whitespace, void)

parseLayoutChar :: Parser TileType
parseLayoutChar = do
  void $ char ' '
  f <- oneOf "#. <^>v"
  return $ read [f]

parseLayoutLine :: Parser [TileType]
parseLayoutLine = whitespace >> void (char '|') >> many1 parseLayoutChar

parseLayout :: Parser Layout
parseLayout = do
  whitespace
  void $ string "- layout"
  field_types <- many1 (try parseLayoutLine)
  let fields_zipped = zip [0 ..] (map (zip [0 ..]) field_types)
  let _tiles =
        [ Tile {tcoordinate = (x, y), ttype = t}
          | (y, line) <- fields_zipped,
            (x, t) <- line
        ]
  return Layout {tiles = _tiles, dimentions = (length $ head field_types, length field_types)}
