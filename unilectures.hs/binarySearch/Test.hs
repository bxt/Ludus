import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck
import Text.Printf

import Tree

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
-- reversing twice a finite list, is the same as sorting
prop_listunlist s = (asList . fromList) s == sort s
    where _ = s :: [Int]

-- showing a single node, is same as surrounding with <>
prop_showsurround s = (show . fromList) [s] == '<' : show s ++ ">"
    where _ = s :: Int

-- smallest of a single node, is the single node
prop_smallestofone s = (smallest . fromList) [s] == fromList [s]
    where _ = s :: Int

-- smallest of a node set, is the first of its sorted list source
prop_smallestofmany s = (value . smallest . fromList) s == (h2 . sort) s
    where _ = s :: [Int]
          h2 [] = Nothing
          h2 x  = Just (head x)

-- found in a node set, iff found in list source 
prop_search s k = (value . search k . fromList) s == find (==k) s
    where _ = (s,k) :: ([Int],Int)

-- contained in a node set, iff its list source contains
prop_contains s k = (contains k . fromList) s == (isJust . find (==k)) s
    where _ = (s,k) :: ([Int],Int)

tests  = [("asList.fromList/sort", quickCheck prop_listunlist)
         ,("smallest/id",        quickCheck prop_smallestofone)
         ,("value.smalilest.fromList/headMaybe.sort",        quickCheck prop_smallestofmany)
         ,("contains k.fromList/isJust.find (==k)",        quickCheck prop_contains)
         ,("value.search k.fromList/find (==k)",        quickCheck prop_search)
         ,("show/<>",        quickCheck prop_showsurround)
         ]
