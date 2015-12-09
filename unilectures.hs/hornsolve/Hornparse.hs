module Hornparse where

import Hornsolve hiding (main)

import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Control.Monad

parseRules :: Parsec String u [Rule Char]
parseRules = concat <$> many (comment <|> line) <* eof
  where comment   = const [] <$> try (string "#") <* anyChar `manyTill` endOfLine
        line      = ruleGroup `sepBy` string " ∧ " <* endOfLine
        ruleGroup = between (string "(") (string ")") rule
        rule      = (~>) <$> head <* string " → " <*> body
        head      = one  <|> upper `sepBy1` try (string " ∧ ")
        one       = const [] <$> char '1'
        body      = zero <|> upper `sepBy1` try (string " ∨ ")
        zero      = const [] <$> char '0'

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = parseFromFile parseRules "input.txt" >>= print . solve . fromRight
