
-- These are some examples for haskell syntax with concise
-- explanations. If you are a real 
-- <abbr title="functional programming">fp</abbr> ninja you might be able
--  to learn haskell by reading this, but it is targeted to haskell learners, 
-- who read a tutorial or book like [LYAH](http://learnyouahaskell.com/ "Learn You a Haskell")
-- and need a reference card to remember the syntax and some patterns.
-- You can load it into `ghci` and run examles and check type signatures.  

{-
 - This is a block comment.
 - 
 - One line comments start with -- 
 -}


-- Module Definition
-- -----------------

-- A module definition has a name andd optionally lists everything you want to export in braces. 

module Cheatsheet

-- Export types with a list of constructors
-- or export all contructors.
  ( Recsam(Recsam)
  , Datasam(..)

-- You can export all values and even reexport imports with`module` here. 
-- The module definition ends with the `where` keyword. 
  , datasam
  , module Control.Monad
  ) where

-- Imports
-- -------

-- Imports load other modules into our scope. 
-- Sometimes you have to import submodules too. 

import Control.Monad
import Control.Monad.Reader

-- You can decide to only load some functions. 
-- Instances and typeclasses are loaded anyways. 

import Data.Function (on)

-- The `qualified` hides the non prefixed versions and `as` renames
-- the prefix. 
-- If you want to define own versions of `Prelude` functions you can
-- use `hiding`.

import qualified Data.Char as C (ord, chr)
import Prelude hiding (gcd)

-- Typeclass
-- ---------

-- A class resembles interfaces and abstract classes of 
-- <abbr title="object-oriented programming">OOP</abbr>. 
-- This one "extends" Num. 
-- Classes list the types of functions to provide using a type variable.
-- Classes may include default implementations. 

class (Num a) => Classam a where
	classam :: a -> Bool
	classam = const False


-- Data
-- ----

-- The data keyword allows definition of algebraic data types. 
-- They can derive `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read`
-- for some convenient instances.
--
-- Datas can have type variables and be recursive. 

data Datasam = A | B | C
  deriving Show

data Datarecsam a = End a | Recur (Datarecsam a)

-- You construct datas using their constructor functions.
-- They can be used like normal functions. 

datasam = A

datarecsam :: [Datarecsam Int]
datarecsam = Recur (End 2) : map (Recur . Recur . End) [3..5]

-- You can make datas instances with a header similar
-- to the class definition. 
-- You do not always have to define all methods. 
-- With this definition we can print the above list like `[>2,>>3,>>4,>>5]`
-- The values are extracted with pattern matching. 

instance Show a => Show (Datarecsam a) where
	show (End x) = show x
	show (Recur xs) = '>': show xs

-- Record Syntax
-- -------------

-- The use of the record syntax ist defining algebraic data types
-- giving meaningful names to their members. 

data Recsam = Recsam { name :: String
                     , hidden :: Bool
                     } deriving Show

-- Reocrd syntax enables easy constructing. Note the `=` in place of `::`. 

recsam = Recsam { name = "Example", hidden = True }

-- The fields of a record are at the same time getter functions. 

recsamName = name recsam

-- You can pattern match againts a record. 
-- If you type `assertShown recsam` in ghci you get the error. 

assertShown Recsam { hidden = True } = error "Not shown"
assertShown _ = ()

-- You can use records to build derivates. 
-- If you type `assertShown recsam'` there's no error.

recsam' = recsam { hidden = False }

-- Newtype
-- -------

-- Newtypes are similar to a type alias, they carry own instances
-- even using `deriving` and can be defined using record syntax, 
-- but are limited to a single field. 

newtype Ntsam = Ntsam [Int]

-- Type
-- ----

-- Type is another alias tool, but only creates a synonyme name and no constructors. 
-- It's most prominent use is `String` for `[Char]`. 

type Pair a = (a,a)
-- Note that you should avoid recursion. The following works however.  
type Coords = Pair Int

-- Special Function Syntax
-- -----------------------

-- There are some advanced function building methods. The examples demonstrate
-- referencing of an infix operator, building lambdas with `\`, partially applying
-- infix operators (sections), using non symbolic functions as infix and defining
-- own infix operators with fixity. They are of questional value here but handy when
-- used right. 

add a b = a + b
add' = (+)
add'' = \a b -> a+b
add3 a b = (+a) b
add4 a b = a `add3` b
a +# b = add a b
infixl 9 +#

-- Pattern matching
-- ----------------

-- There are pattern matchng examples for function definitions in the 
-- sections about `data` and the record syntax. 
-- There's more options though. You can use patterns in `let`, 
-- `case`, lambdas, with `<-` and `where` too. 

patternsam x y = 
  let (a,b) = (,) x y
  in case a of 3     -> b
               (n+1) -> n
-- You can pattern match tuples, lists and sums with their syntactic
-- sugar in adition to normal constructors. You can use the `@` notation
-- to match a parameter while keeping it intact. 

single3 l@[3] = Just l
single3    _  = Nothing

-- Let & where, if, case & guards
-- --------------------------------

-- If you just want to check constructors use pattern matching, see above. 
-- Sometimes you want to use complex predicates though. You can choose guards. 
-- With a `where` block you can define private functions. 

numsam x
       | x < 3     = phrase "small"
       | x < 50    = phrase "medium"
       | otherwise = phrase "huge"
       where
         phrase s = "A "++s++" number!"

-- A little mothe flexible are `let` and `case` because they are expresseions
-- and can be used everywhere as opposed to only in function definitions. 
-- A little hack is to case on the unit `()` only matching the ignore `_` 
-- pattern thus using only the guards. 

numsam' x
  = let phrase s = "A "++s++" number!"
    in case () of
            _ | x < 3     -> phrase "small"
              | x < 50    -> phrase "medium"
              | otherwise -> phrase "huge"

-- And of course there is `if then else` working a little 
-- like the ternary operator `a?b:c` in other languages. 
-- You can use multiple `else if` branches but no pattern matching. 

numsam'' x = phrase $
              if      x < 3  then "small"
              else if x < 50 then "medium"
              else                "huge"
    where phrase s = "A "++s++" number!"

-- List comprehensions
-- -------------------

-- An option for filtering and processing lists are list comprehensions. 
-- They take multiple input lists, predicates and an output expression. 

listcmrsam = [(x,show y) | x <- [0..3], y <- [3..5], x*y < 4]

-- Monads
-- ------

-- The above list comprehension can be written in monadic style too. 

monsam = [0..3] >>= (\x -> 
  [3..5] >>= (\y -> 
    guard (x*y < 4) >> return (x,show y) ) )

-- Do
-- --

-- When dealing with monads using the `do` notation can sometimes be 
-- handy albait considered harmful. Inside a `do` block a failed
-- pattern match will call `fail` with an exception message. 
--
-- The first non whitespace char determines the width of indentation. 

dosam = do
  x <- [0..3]
  y <- [3..5]
  guard $ x*y < 4
  return (x, show y)


-- IO
-- --

-- The programm listed here defines a function `askNumber` to read 
-- a number > 3 from user input and then outputs some numbered lines. 
-- I try to keep the IO parts as short as possible. 
--
-- Also interesting for simple <abbr title="input/output">I/O</abbr> 
-- are `interact`, `print` and `System.Environment (getArgs)`

main = do
  times <- askNumber
  sequence_ [putStrLn $ "line" ++ show n | n <- [0..times-1]]

askNumber :: IO Int
askNumber = do
  putStrLn "How many?"
  n <- readLn
  if n>3 then return n
    else do
      print "To few!"
      askNumber


-- Pointfree Style
-- ---------------

-- Here are some patterns for using pointfree style. 

-- For comparing stuff use the `on` comosition
onsam x y = x*3 + y*3
onsam' = (+) `on` (*3)

-- If you want to combine functions with arity one and two you can 
-- sometimes use the boobs operator. 
(.:) = (.).(.)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- Here's an example how to use the boobs operator with Y pipes:
ysam pe c pd= pe .: (c `on` pd) where
tapas = ysam  ('T':) (++) ('a':)  "p" "s"

-- If you have two different pipes for one input and a combinator you use `liftM2`. 
liftM2sam pe c p1 p2  = pe . liftM2 c p1 p2
tapas' = liftM2sam (++"s") (++) ('T':) ('p':)  "a"

-- If you just want to clone an argument you can use join. 
joinsam = join (+)
fiveDoubled = joinsam 5

-- If you have two different pipes for two different arguments and a combinator you use
-- composition with a pre applyed function. 

applysam c p1 p2 = (. p2) . c . p1
tacos = applysam (++) ("Ta"++) ('o':)  "c" "s"

-- Links
-- -----

-- Here's some useful external references:
--
-- * [Hoogle](http://www.haskell.org/hoogle/)
-- * [Haskell Hierarchical Libraries](http://www.haskell.org/ghc/docs/latest/html/libraries/)
-- * [Haskell Toolchain](http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program "How to write a Haskell program")
-- * [Prelude](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html)
-- * [Haskell 98 Report](http://www.haskell.org/onlinereport/)
-- * [Haddock Markup](http://www.haskell.org/haddock/doc/html/ch03s08.html)
-- * [Planet Haskell](http://planet.haskell.org/)
-- * [Real World Haskell](http://book.realworldhaskell.org/read/)
-- * [Learn You a Haskell](http://learnyouahaskell.com/chapters)
-- * [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia)
-- * [Gentle Introduction To Haskell](http://www.haskell.org/tutorial/index.html)
--
--

-- Meta
-- ----

-- This file was written by [Bernhard Häussner](http://bernhardhaeussner.de) and
-- processed by [docco](http://jashkenas.github.com/docco/). 







