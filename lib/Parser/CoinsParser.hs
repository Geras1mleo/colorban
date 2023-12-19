module Parser.CoinsParser (parseCoins) where

import Data (Coin (..))
import Parser.MyParser (Parser, parseArray, parseFields1, parseName)

getCoin :: Coin
getCoin =
  Coin
    { coinName = undefined,
      coinCoordinate = undefined,
      value = undefined
    }

setCoinField :: Coin -> (String, String) -> Coin
setCoinField coin ("position", value) = coin {coinCoordinate = read ("(" ++ value ++ ")")}
setCoinField coin ("value", value') = coin {value = read value'}
setCoinField _ (key, _) = error ("Undefined Coin key: \"" ++ key ++ "\"")

parseCoin :: Parser Coin
parseCoin = do
  name <- parseName
  let coin = getCoin {coinName = name}
  foldl setCoinField coin <$> parseFields1

parseCoins :: Parser [Coin]
parseCoins = parseArray "coins" parseCoin
