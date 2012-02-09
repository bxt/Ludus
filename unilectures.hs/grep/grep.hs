import System.Environment

import Data.List (isInfixOf)

main = do
  args <- getArgs
  interact $ unlines . filter (isInfixOf $ head args) . lines
