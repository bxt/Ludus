module Base where

data Player = CliPlayer String | Idiot | CpuPlayer deriving Show

type Heap = Int

type Heaps = [Heap]

data Game = Game { heaps :: Heaps, players :: [Player] }

player :: Game -> Player
player = head . players

nextPlayer :: Game -> Game
nextPlayer g = g { players = rotate $ players g } where
  rotate (x:xs) = xs ++ [x]

type Move = ( Int -- ^ heap from which to take the pieces
            , Int -- ^ amount of pieces to take away
            )
