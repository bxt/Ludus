import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

check ('(':xs) ys = check xs (():ys)
check (')':xs) (_:ys) = check xs ys
check (')':xs) _ = False
check (_:xs) ys = check xs ys
check [] [] = True
check _ _ = False



check2 xs = maybe False ((==0).snd) $ runStateT (foldM_ f ' ' xs) 0
  where
    f :: Char -> Char -> StateT Int Maybe Char
    f c '(' = do
      modify (+1)
      return c
    f c ')' =  do
      modify $ subtract 1
      n <- get
      if n<0
        then fail "too many closing"
        else return c
    f c _   = return c


