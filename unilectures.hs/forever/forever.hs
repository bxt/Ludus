
import Control.Monad (forever,(=<<))
import Control.Applicative
import Data.Char

-- | Convert a number to a list of digits to a base
toBase :: (Integral t) => t   -- ^ Base
                       -> t   -- ^ Input number
                       -> [t] -- ^ Output digit list
toBase b = λ [] where
  λ xs 0 = xs
  λ xs v = λ (r:xs) q
    where (q,r) = v `divMod` b


-- | Convert a of digits to a string
showBase = (=<<) λ where
  λ x
    | x < 10    = show x
    | otherwise = ((:[]).chr.(+55)) x

-- | Check if a number is a palindrome in a base
isPalindromeInBase :: Int  -- ^ Base 
                   -> Int  -- ^ Number to check
                   -> Bool -- ^ If or not palindromic
isPalindromeInBase = ((<*>) . (((==) . reverse) .) . toBase) <*> toBase

μ :: Int -> Int
μ = (head (filter (and . (<**> (isPalindromeInBase <$> [5,9])) . pure) [10..]) +)

{- Alternate:

μ = (head (filter (\x -> all ($x) (isPalindromeInBase <$> [5,9])) [10..]) +)

μ = (head (filter (\x -> and $ [x] <**> (isPalindromeInBase <$> [5,9])) [10..] ) +)

μ = (head (filter (and . flip map paliBase59 . flip id) [10..]) +)
   where paliBase59 = isPalindromeInBase <$> [5,9]

-}

main :: IO b
main = forever.putStrLn $ '3': (<$>) (chr.μ) [8,5,1,12]


