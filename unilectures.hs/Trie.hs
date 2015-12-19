module Trie where

import qualified Data.Map.Lazy as Map
import Data.Function (on)
import Data.Maybe (isJust, fromJust)
import Control.Monad

data Trie a = Trie { untrie :: Map.Map a (Trie a) } deriving (Show, Eq)

instance Ord a => Monoid (Trie a) where
  mempty = Trie Map.empty
  mappend = (Trie .) . Map.unionWith mappend `on` untrie

asList :: Eq a => Trie a -> [[a]]
asList = asMonoids (:[])

size :: Eq a => Trie a -> Int
size = length . asMonoids (const ())

asMonoids :: (Monoid m, Eq m) => (a -> m) -> Trie a -> [m]
asMonoids f = ensure . Map.foldMapWithKey aux . untrie where
  aux a = map (f a `mappend`) . asMonoids f

ensure :: (Monoid m, Eq m) => [m] -> [m]
ensure x | x == []   = [mempty]
         | otherwise = x

fromList :: (Foldable f, Foldable g, Ord a) => f (g a) -> Trie a
fromList = foldr insert mempty where

insert :: (Foldable f, Ord a) => f a -> Trie a -> Trie a
insert = mappend . singleton

singleton :: (Foldable f, Ord a) => f a -> Trie a
singleton f = pushPrefix f mempty

null :: (Ord a, Eq a) => Trie a -> Bool
null = (== mempty)

elem :: (Foldable f, Ord a, Eq a) => f a -> Trie a -> Bool
elem = (maybe False Trie.null . ) . stripPrefix

isPrefixIn :: (Foldable f, Ord a, Eq a) => f a -> Trie a -> Bool
isPrefixIn = (isJust .) . stripPrefix

dropPrefix :: (Foldable f, Ord a, Eq a) => f a -> Trie a -> Trie a
dropPrefix = (fromJust .) . stripPrefix

stripPrefix :: (Foldable f, Ord a, Eq a) => f a -> Trie a -> Maybe (Trie a)
stripPrefix f t = foldM (flip walk) t f

walk :: (Ord a, Eq a) => a -> Trie a -> Maybe (Trie a)
walk a t = Map.lookup a $ untrie t

drop :: (Ord a, Eq a) => a -> Trie a -> Trie a
drop = (fromJust .) . walk

pushPrefix  :: (Foldable f, Ord a, Eq a) => f a -> Trie a -> Trie a
pushPrefix f t = foldr aux t $ f where
  aux a t = Trie $ Map.singleton a t
