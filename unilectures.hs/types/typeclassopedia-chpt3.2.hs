
-- Exercise 1

instance Functor (Either e) where
	fmap g (Right a) = Right . g $ a
        fmap _ (Left e) = Left e

instance Functor ((->) e) where
	fmap g f = g . f

-- Exercise 2

instance Functor ((,) e) where
	fmap g (e,a) = (e,g a)

data Pair a = Pair a a
	deriving Show

instance Functor Pair where
	fmap g (Pair a a') = Pair (g a) (g a')

-- Exercise 3

data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

instance Functor ITree where
	fmap g (Leaf h) = Leaf $ fmap g h
	fmap g (Node l) = Node $ fmap (fmap g) l

-- Exercise 4

-- (a -> String)

-- Exercise 5

newtype FunC f f' a = FunC { runFunC :: f (f' a) }
	deriving Show

instance (Functor f, Functor f') => Functor (FunC f f') where
 	fmap g  = FunC . fmap (fmap g) . runFunC








