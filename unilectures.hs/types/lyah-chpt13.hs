
import Data.Monoid

isBigGang x = (x > 9, "Gruppenstaerke mit 9 verglichen. ")

applyLog :: (Monoid c) => (a,c) -> (a -> (b,c)) -> (b,c)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

platoon = (30, "Eine ganze Kompanie. ") `applyLog` isBigGang

suhm :: (Int,Product Int)
suhm = ("abcd",Product 5) `applyLog` (\x -> (length x,Product 3))

newtype Writer w a = Writer { runWriter :: (a,w) }

instance (Monoid w) => Monad (Writer w) where
  (Writer (a,w)) >>= f = let Writer (b,w') = f a in Writer (b, w `mappend` w')
  return x = Writer (x,mempty)


isBigGang' x = Writer (x > 9, "Gruppenstaerke mit 9 verglichen. ")

platoon' = Writer (30, "Eine ganze Kompanie. ") >>= isBigGang'

tell :: w -> Writer w ()
tell w = Writer ((),w)

gotIt = platoon' >> tell "Ack! "

logNumber :: (Show a) => a -> Writer [String] a
logNumber x = Writer (x, ["Bekommen: "++show x])

logNumber' x = do
  tell ["Bekommen': "++show x]
  return x

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber' 2
  tell ["Werde Multiplizieren..."]
  return (a*b) 






gcd' a b 
  | b == 0 = do
      tell (["Beendet bei a = "++show a]++)
  | otherwise = do
      let amb = a `mod` b
      result <- gcd' b amb
      tell ((show a++" % "++show b++" = "++show amb):)
      return result

printGcd :: IO ()
printGcd = mapM_ putStrLn . ($[]) . snd . runWriter $ gcd' 60 42


-- This is slow...

finalCountDown :: Int -> Writer ([String] -> [String]) ()
finalCountDown 0 = do
  tell (["0"]++)
finalCountDown  x = do
  finalCountDown (x-1) 
  tell ([show x]++)

finalCountDownLog = mapM_ putStrLn . ($[]) . snd . runWriter . finalCountDown 

-- This is slow by design:

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x-1)
  tell [show x]

finalCountDownLog' = mapM_ putStrLn . snd . runWriter . finalCountDown'

-- This is fast :o

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

finalCountDown'' :: Int -> Writer (DiffList String) ()
finalCountDown'' 0 = do
  tell (toDiffList ["0"])
finalCountDown'' x = do
  finalCountDown'' (x-1)
  tell (toDiffList [show x])

finalCountDownLog'' = mapM_ putStrLn . fromDiffList . snd . runWriter . finalCountDown''

-- main = finalCountDiownLog'' 10000








