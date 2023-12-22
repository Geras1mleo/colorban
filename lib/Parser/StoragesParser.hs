module Parser.StoragesParser (parseStorages) where

import GameData(Storage (..))
import Parser.MyParser (Parser, parseArray, parseFields1, parseName)

getStorage :: Storage
getStorage =
  Storage
    { sname = undefined,
      scoordinate = undefined,
      scolor = undefined
    }

setStorageField :: Storage -> (String, String) -> Storage
setStorageField storage ("position", value) = storage {scoordinate = read ("(" ++ value ++ ")")}
setStorageField storage ("color", value) = storage {scolor = read value}
setStorageField _ (key, _) = error ("Undefined storage key: \"" ++ key ++ "\"")

parseStorage :: Parser Storage
parseStorage = do
  name <- parseName
  let storage = getStorage {sname = name}
  foldl setStorageField storage <$> parseFields1

parseStorages :: Parser [Storage]
parseStorages = parseArray "storages" parseStorage
