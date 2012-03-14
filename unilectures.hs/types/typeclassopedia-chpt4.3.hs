
import Control.Applicative

-- Exercise 1

data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
	fmap g (Just' a) = Just' $ g a
	fmap _ _ = Nothing'

instance Applicative Maybe' where
	pure = Just'
	(Just' g) <*> (Just' a) = Just' $ g a
	_ <*> _ = Nothing'



