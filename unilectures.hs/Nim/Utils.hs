module Utils where

-- | Update a value at a specific index with the result of the provided function.
--   When the indext is out of bounds, the original list is returned.
--
-- >>> adjust (+10) 1 [1,2,3]
-- [1,12,3]
--
-- >>> adjust succ 0 "Faum"
-- "Gaum"
--
-- >>> adjust (+10) 4 [1,2,3]
-- [1,2,3]
--
-- >>> adjust (+10) (-3) [1,2,3]
-- [1,2,3]
adjust :: (a -> a) -> Int -> [a] -> [a]
adjust _ _ []                 = []
adjust f n (x:xs) | n < 0     = x : xs
                  | n == 0    = f x : xs
                  | otherwise = x : adjust f (pred n) xs

-- | Pad a list to a given length.
--
-- >>> padTo 'x' 3 "aaa"
-- "aaa"
--
-- >>> padTo 'x' 3 "a"
-- "xax"
--
-- >>> padTo 'x' 3 "aa"
-- "aax"
padTo :: a -> Int -> [a] -> [a]
padTo with to xs = replicate a with ++ xs ++ replicate b with where
  l = length xs
  a = (to-l) `div` 2
  b = to - l - a
