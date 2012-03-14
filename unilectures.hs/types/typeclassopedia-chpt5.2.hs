
-- Exercise 1

data List a = Nil | Cons a (List a)
	deriving Show

instance Monad List where 
	return a = Cons a Nil
	Nil >>= _ = Nil
	(Cons x xs) >>= g = g x `append` (xs >>= g)
	fail _ = Nil

append l Nil = l
append Nil l' = l'
append (Cons x xs) l' = Cons x (xs `append` l')

-- Exercise 3

data Free f a = Var a
              | Node (f (Free f a))

instance Functor f => Functor (Free f) where
	fmap g (Var a) = Var $ g a
	fmap g (Node xs) = Node $ fmap (fmap g) xs

