module Parser.CratesParser (parseCrates) where

import GameData(Crate (..))
import Parser.MyParser (Parser, parseArray, parseFields1, parseName, readDouble)

getCrate :: Crate
getCrate =
  Crate
    { cname = undefined,
      ccoordinate = undefined,
      ccolor = undefined,
      weight = undefined
    }

setCrateField :: Crate -> (String, String) -> Crate
setCrateField crate ("position", value) = crate {ccoordinate = read ("(" ++ value ++ ")")}
setCrateField crate ("color", value) = crate {ccolor = read value}
setCrateField crate ("weight", value) = crate {weight = readDouble value}
setCrateField _ (key, _) = error ("Undefined Crate key: \"" ++ key ++ "\"")

parseCrate :: Parser Crate
parseCrate = do
  name <- parseName
  let crate = getCrate {cname = name}
  foldl setCrateField crate <$> parseFields1

parseCrates :: Parser [Crate]
parseCrates = parseArray "crates" parseCrate
