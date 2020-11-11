module Hornsolve where

import Data.Set (Set)
import qualified Data.Set as Set

type Rule a = (Set a, Set a)

-- | Helper for constructing implications from body a and head b
a ~> b = (Set.fromList a, Set.fromList b)

-- | List of models from list of implications
solve :: (Ord a) => [Rule a] -> [[a]]
solve rules = map Set.toList $ step Set.empty where
  step set = if null mr then [set] else ((=<<)step . map (`Set.insert` set) . Set.toList . snd . head) mr
    where mr = filter (Set.null . Set.intersection set . snd) . filter ((`Set.isSubsetOf` set) . fst) $ rules

-- | Is there at least one model
unsolvable :: (Ord a) => [Rule a] -> Bool
unsolvable = null . solve

-- | Sample list of implications
rules = [""~>"AB","A"~>"CE","AC"~>"D","B"~>"CE","E"~>""] :: [Rule Char]

main = print $ solve rules
