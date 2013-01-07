import Control.Monad

divisors 1 = [1]
divisors n = (1:filter ((==0) . rem n) [2..n `div` 2]) ++ [n]

info x = (x, length $ divisors x, divisors x)

infolist = map info [1..]

onlybig [] _ = []
onlybig (x@(_,dC,_):xs) pdC = if dC > pdC then x:onlybig xs dC else onlybig xs pdC

main = forM_ (onlybig infolist 0) (putStrLn.show)