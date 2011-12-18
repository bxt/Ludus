module Tree where

import Data.List (intercalate)

data Tree a = Nil | Node a (Tree a) (Tree a) (Tree a) deriving (Read, Eq)

-- insert x Nil = Node x Nil Nil
-- insert x (Node a left right)

value (Node a _ _ _) = a

left (Node _ left _ _) = left

right (Node _ _ right _) = right

parent (Node _ _ _ parent) = parent

-- "InorderTreeWalk"
asList Nil = []
asList (Node a left right _) = (asList left)++[a]++(asList right)

-- "Search"
search x Nil = Nil
search x n@(Node a left right _)
  | x==a = n
  | x<a  = search x left
  | x>a  = search x right

contains x t = case search x t of { Nil -> False; otherwise -> True }

-- "Minimum"
smallest Nil = Nil
smallest n@(Node _ Nil _ _) = n
smallest (Node _ left _ _) = smallest left

-- "Successor"
next Nil = Nil
next (Node _ _ Nil Nil) = Nil
next n@(Node _ _ Nil _) = moveUp n
  where
    moveUp n@(Node _ _ _ parent@(Node _ _ Nil _)) = parent
    moveUp n@(Node _ _ _ parent@(Node _ _ pRight _))
      | pRight==n = moveUp parent
      | otherwise = n
next (Node _ _ right _) = right

-- "Insert"
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


fromList xs = foldr insert Nil xs


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

instance (Show a) => Show (Tree a) where
  show x = intercalate "\n" $ fst $ toString x


