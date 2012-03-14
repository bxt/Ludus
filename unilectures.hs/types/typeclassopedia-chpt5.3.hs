
import Control.Monad

-- Exercise 1


m `bind` f = join . fmap f $ m


-- Exercise 2

join' mm = mm >>= id
fmap' g m = m >>= (return.g)


