
-- | This module contains a simple implementation of a binary search tree
module Tree where

import Data.List (intercalate)

-- * Data

data Tree a 
  -- | Null pointer fe leave's children and empty tree
  = Nil
  -- | Actual node containing a vaule and left, right, parent nodes
  | Node a (Tree a) (Tree a) (Tree a) deriving (Read, Eq)

-- * Getters

-- | Contained element of a tree node
value :: Tree a -> Maybe a
value Nil = Nothing
value (Node a _ _ _) = Just a

-- | Left (smaller) child of a tree node
left :: Tree a -> Tree a
left Nil = Nil
left (Node _ left _ _) = left

-- |  Right (greater) child of a tree node
right :: Tree a -> Tree a
right Nil = Nil
right (Node _ _ right _) = right

-- | Parent node of a tree node
parent :: Tree a -> Tree a
parent Nil = Nil
parent (Node _ _ _ parent) = Nil

-- * Constructor


-- | Create a tree from a list of values, 
--   search time depends on insertion order
fromList :: (Ord a) => [a] -> Tree a
fromList xs = foldr insert Nil xs
-- >>> fromList [3,1,2]
--    <2>   
-- <1>   <3>

-- * Conversion

-- | Return whole tree as a list, 
--   similar to InorderTreeWalk
asList :: Tree a -> [a]
asList Nil = []
asList (Node a left right _) = (asList left)++[a]++(asList right)

-- * Traversing

-- | Search for node with certain value in the tree, 
--   similar to Search
search :: (Ord a) => a -> Tree a -> Tree a
search x Nil = Nil
search x n@(Node a left right _)
  | x==a = n
  | x<a  = search x left
  | x>a  = search x right

-- | Same as `search` but retruning a truth value
contains :: (Ord a) => a -> Tree a -> Bool
contains x t = case search x t of { Nil -> False; otherwise -> True }

-- | Get the node with lowest value, 
--   similar to Minimum
smallest :: Tree a -- ^ Tree to search in
         -> Tree a -- ^ Node with smallest value
smallest Nil = Nil
smallest n@(Node _ Nil _ _) = n
smallest (Node _ left _ _) = smallest left

-- | not working - should return the next greater value containing node, 
--   similar to Successor
next :: (Eq a) => Tree a -> Tree a
next Nil = Nil
next (Node _ _ Nil Nil) = Nil
next n@(Node _ _ Nil _) = moveUp n
  where
    moveUp n@(Node _ _ _ parent@(Node _ _ Nil _)) = parent
    moveUp n@(Node _ _ _ parent@(Node _ _ pRight _))
      | pRight==n = moveUp parent
      | otherwise = n
next (Node _ _ right _) = right

-- * Modification

-- | Add an element into the tree, 
--   similar to Insert
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Nil = Node x Nil Nil Nil
insert x n@(Node _ _ _ parent)  = innerInsert n parent
  where
    innerInsert Nil outerParent = Node x Nil Nil outerParent
    innerInsert (Node a left right parent) outerParent
      | x < a = insertLeft
      | otherwise = insertRight
      where
       insertLeft = new where new =  Node a (innerInsert left new) right parent
       insertRight = new where new = Node a left (innerInsert right new) parent


instance (Show a) => Show (Tree a) where
  show x = intercalate "\n" $ fst $ toString x
    where
      toString :: (Show t) => Tree t -> ([[Char]], Int)
      toString Nil = ([],0)
      toString (Node value left right _) = (strings,count)
        where strings = ((spaces lC)++me++(spaces rC)) : zipWithPad (\x y -> x ++ (spaces meC) ++ y ) left' right'
              count = lC+rC+meC
              (left',lC)  = toString left
              (right',rC) = toString right
              me = "<"++show value++">"
              meC = (length me)
              spaces n = take n $ repeat ' '
              zipWithPad f [] [] = []
              zipWithPad f xs ys = (f (nxt xs) (nxt ys)) : zipWithPad f (rest xs) (rest ys)
              nxt [] = ""
              nxt xs = head xs
              rest [] = []
              rest xs = tail xs


