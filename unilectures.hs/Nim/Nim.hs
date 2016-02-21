module Nim where

import Control.Monad
import Control.Monad.Except

import Utils

data Player = Player1 | Player2 deriving Show

otherPlyer :: Player -> Player
otherPlyer Player1 = Player2
otherPlyer Player2 = Player1

data Game = Game { heaps :: [Int], player :: Player }

newGame :: Game
newGame = Game { heaps = [1,3,5,7], player = Player1 }

instance Show Game where
  show g = concatMap aux (zip [1..] $ heaps g) ++ show (player g) where
    aux (i,a) = show i ++ " " ++ padTo ' ' m (replicate a '|') ++ "\n"
    m = maximum $ heaps g

move :: Int -> Int -> Game -> Either String Game
move h a g = do
  let hs = heaps g
  when (h > length hs)
    $ Left "Heap number too high"
  when (h <= 0)
    $ Left "Heap number too low"
  when (a <= 0)
    $ Left "Must take at least one piece"
  when (a > (hs !! pred h))
    $ Left $ "Can only take " ++ show (hs !! h) ++ " pieces"
  return g { heaps = filter (/=0) $ adjust (subtract a) (pred h) hs }

nim :: Game -> IO()
nim g = do
  putStr "\n" >> print g
  h <- readLnWith " enter a heap: "
  a <- readLnWith " and enter an amount: "
  let eg = move h a g
  case eg of (Left e)   -> print e >> nim g
             (Right g') -> winCheck g'
  where
    winCheck g   = if null (heaps g) then winning (player g) else nextTurn g
    winning p    = putStr $ "\n  +++ Congratulations, you won " ++ show p ++ "! +++\n"
    nextTurn g   = nim $ g { player = otherPlyer (player g) }
    readLnWith s = putStr s >> (readLn `catchError` const (readLnWith s))

main :: IO()
main = do
  putStrLn "\n  +++ Welcome to NIM +++"
  nim newGame
