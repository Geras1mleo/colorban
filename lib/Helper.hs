module Helper ((!?)) where

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/src/GHC.List.html#%21%3F
(!?) :: [a] -> Int -> Maybe a
{-# INLINEABLE (!?) #-}
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n
