
-- Exercise 1

data Fail a = Fail a
	deriving Show

instance Functor Fail where
	fmap g (Fail a) = Fail (g $! a)

-- fmap id (Fail 3) == Fail 3
--
-- fmap (const () . undefined) (Fail 'F') == Fail ()
-- but
-- fmap (const ()) . fmap undefined $ (Fail 'F') --> Fail *** Exception: Prelude.undefined

-- Exercise 2

-- Both:

-- fmap id [()] = [(),()] <> [()] = id [()]
--
-- fmap (id.id) [()] = fmap id [()] = [(),()] <> [(),(),(),()] = fmap id [(),()] = (fmap id . fmap id) [(),()]

