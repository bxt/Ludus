module Sorts where

-- note: you can run the examples and props with doctest

-- $setup
-- >>> import Data.List (sort)

-- | Put a number in the correct position of a presorted list
--
-- >>> insert 3 [1,2,4,5]
-- [1,2,3,4,5]
--
-- prop> elem x $ insert x xs
-- prop> insert x (sort xs) == sort (x:xs)
insert :: Int -> [Int] -> [Int]
insert n []     = [n]
insert n (x:xs) | n > x     = x : insert n xs
                | otherwise = n : x : xs

-- | Sort a list using insertion sort
--
-- >>> isort [2,5,4,1,3]
-- [1,2,3,4,5]
--
-- prop> isort xs == sort xs
isort :: [Int] -> [Int]
isort [] = []
isort xs = foldr insert [] xs

-- | Merge two sorted lists
--
-- >>> merge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
--
-- prop> merge (sort xs) (sort ys) == sort (xs ++ ys)
merge :: [Int] -> [Int] -> [Int]
merge xs     []     = xs
merge []     xs     = xs
merge (x:xs) (y:ys) = if y > x then x : merge xs (y:ys) else y : merge (x:xs) ys

-- | Sort a list using merge sort
--
-- >>> msort [2,5,4,1,3]
-- [1,2,3,4,5]
--
-- prop> msort xs == sort xs
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = let n = length xs `div` 2
            in merge (msort $ take n xs) (msort $ drop n xs)
