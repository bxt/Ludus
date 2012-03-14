
-- see http://blog.sigfpe.com/2007/04/trivial-monad.html

data W a = W a deriving Show

instance Monad W where
  -- return :: a -> W a
  return = W
  -- bind :: (a -> W b) -> (W a -> W b)
  (W x) >>= f = f x

instance Functor W where
  -- fmap :: (a -> b) -> (W a -> W b)
  fmap f (W x) = W (f x)

-- Exercise 1

g x wy = wy >>= (\y -> return (y+x))

-- Exercise 2

h wx wy = do
  x <- wx
  y <- wy
  return $ x+y

-- Exercise 4

join wwx = wwx >>= id

