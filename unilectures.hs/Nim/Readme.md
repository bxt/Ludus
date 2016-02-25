Nim Game
========

This is an implementation of the game [Nim](https://en.wikipedia.org/wiki/Nim).
You can compile it e.g. by using `ghc Main.hs -o nimgame`. The game will start
with the AI taking a random move. Then you can start to play by entering a heap
and a number of pieces to take from the heap. You take turns with the AI taking
pieces. Whoever takes the last piece wins.

A game could look like this:

    > ./nimgame

      +++ Welcome to NIM +++
      (try to take the last piece)

     enter your name:
    Bernhard

    1    |
    2   |||
    3  |||||
    4 |||||||
    CPU player takes their move... (2,3)

    1    |
    2  |||||
    3 |||||||
    Bernhard
     enter a heap:
    3
     and enter an amount:
    3

    1   |
    2 |||||
    3 ||||
    CPU player takes their move... (2,4)

    1  |
    2  |
    3 ||||
    Bernhard
     enter a heap:
    3
     and enter an amount:
    4

    1 |
    2 |
    CPU player takes their move... (2,1)

    1 |
    Bernhard
     enter a heap:
    1
     and enter an amount:
    1

      +++ Congratulations, you won Bernhard! +++

    __      _____| | |   __| | ___  _ __   ___
    \ \ /\ / / _ \ | |  / _` |/ _ \| '_ \ / _ \
     \ V  V /  __/ | | | (_| | (_) | | | |  __/
      \_/\_/ \___|_|_|  \__,_|\___/|_| |_|\___|


    Play again? (y/n)
    n
    >
