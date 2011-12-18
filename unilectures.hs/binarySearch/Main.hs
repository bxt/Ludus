module Main where

import Tree

x = fromList [12,31,52,74,27,63,45,96,118,131,152,109,148,125,81]
main = do
  putStrLn "The tree:"
  print $ x
  putStrLn "As list"
  print $ asList x
  putStrLn "Filtered items:"
  print $ filter ((flip contains) x) [1..200]
  putStrLn "Smallest item:"
  print $ smallest x
