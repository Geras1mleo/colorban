module Helper ((!?), replaceFirst) where

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

-- https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
replaceFirst :: (Eq a) => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst old new (x : xs)
  | x == old = new : xs
  | otherwise = x : replaceFirst old new xs