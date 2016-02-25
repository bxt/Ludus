module CpuPlayer (runCpuPlayer) where

import Data.Bits (xor)
import Data.List (findIndices)
import Base

-- | A list of pissbilbe winning moves in a game of Nim
--
-- >>> runCpuPlayer [3,4,5]
-- [(1,2)]
runCpuPlayer :: Heaps -> [Move]
runCpuPlayer hs = filter ((>0) . snd) $ zip [1..] $ map aux hs
  where
    aux h = h - (h `xor` s)
    s  = foldr1 xor hs
