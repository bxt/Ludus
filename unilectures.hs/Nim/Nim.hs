module Nim where

import Control.Monad
import System.Random (randomRIO)

import Utils
import Base
import CpuPlayer

randomPickIO :: [a] -> IO a
randomPickIO xs = fmap (xs !!) $ randomRIO (0, pred $ length xs)

newGame :: [Player] -> Game
newGame ps = Game { heaps = [1,3,5,7], players = ps }

showHeaps :: Heaps -> String
showHeaps hs = concatMap aux $ zip [1..] $ hs where
    aux (i,h) = show i ++ " " ++ padTo ' ' m (replicate h '|') ++ "\n"
    m = maximum hs

possibleMoves :: Heaps -> [Move]
possibleMoves hs = concatMap aux $ zip [1..] hs where
  aux (i,h) = [(i,a) | a <- [1..h] ]

runMove :: Move -> Heaps -> Either String Heaps
runMove (h,a) hs = do
  when (h > length hs)
    $ Left "Heap number too high"
  when (h <= 0)
    $ Left "Heap number too low"
  when (a <= 0)
    $ Left "Must take at least one piece"
  when (a > (hs !! pred h))
    $ Left $ "Can only take " ++ show (hs !! h) ++ " pieces"
  return $ filter (/=0) $ adjust (subtract a) (pred h) hs

runPlayer :: Player -> Heaps -> IO Move
runPlayer (CliPlayer name) hs = do
  putStrLn name
  h <- readLnWith " enter a heap: "
  a <- readLnWith " and enter an amount: "
  return (h,a)
runPlayer Idiot            hs = do
  putStrLn "Idiot player takes away first piece..."
  return (1,1)
runPlayer CpuPlayer        hs = do
  putStrLn "CPU player takes their move..."
  let opts = runCpuPlayer $ hs
  randomPickIO $ if null opts then possibleMoves hs else opts

nim :: Game -> IO()
nim g = do
  putStr "\n"
  putStr $ showHeaps $ heaps g
  m <- runPlayer (player g) (heaps g)
  let eg = runMove m (heaps g)
  case eg of (Left  e ) -> print e >> nim g
             (Right hs) -> if null hs
               then winning (player g)
               else nextTurn g { heaps = hs }
  where
    nextTurn = nim . nextPlayer

winning :: Player -> IO()
winning (CliPlayer name) = putStr $ "\n  +++ Congratulations, you won " ++ name ++ "! +++\n"
winning Idiot            = putStr   "\n  +++ Wow, you lost. +++\n"
winning CpuPlayer        = putStr   "\n  +++ Game over! +++\n"

main :: IO()
main = do
  putStrLn "\n  +++ Welcome to NIM +++"
  putStrLn " (try to take the last piece)"
  nim $ newGame [CpuPlayer, CliPlayer "Bernhard"]
