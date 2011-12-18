module Main where

import Tree

-- | Example tree created with `fromList`
x :: Tree Integer
x = fromList [12,31,52,74,27,63,45,96,118,131,152,109,148,125,81]

-- | Method to print example tree `x`, convert it to 
--   list with `asList`, call `smallest` and do some 
--   searching with `contains`
main :: IO ()
main = do
  putStrLn "The tree:"
  print $ x
  putStrLn "As list"
  print $ asList x
  putStrLn "Filtered items:"
  print $ filter ((flip contains) x) [1..200]
  putStrLn "Smallest item:"
  print $ smallest x
