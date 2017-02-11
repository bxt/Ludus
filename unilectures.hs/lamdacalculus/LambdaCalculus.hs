import Text.Parsec
import Text.Parsec.String (parseFromFile)
import Control.Monad

import Control.Exception.Base (assert)

type Identifier = String

data Expression = Identifier Identifier
                | Abstraction Identifier Expression
                | Application Expression Expression
                deriving Eq

instance Show Expression where
  show (Identifier i)      = i
  show (Abstraction i e)   = "λ " ++ i ++ " . " ++ show e
  show (Application e1 e2) = show' e1 ++ " " ++ show e2 where
    show' (Identifier i)   = i
    show' e                = "(" ++ show e ++ ")"

parseExpressions :: Parsec String u [Expression]
parseExpressions = many (expression <* endOfLine) <* eof
  where expression   :: Parsec String u Expression
        expression   = expression' `chainl1` (space >> return Application)
        space        = string " "
        expression'  = abstraction <|> parens <|> identifierEx
        identifierEx = Identifier <$> identifier
        identifier   = many1 $ noneOf " \n()lλ"
        abstraction  = Abstraction <$> (lambda *> identifier) <* dot <*> expression
        lambda       = string "l " <|> string "λ "
        parens       = string "(" *> expression <* string ")"
        dot          = string " . "

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

loadExpressions :: String -> IO [Expression]
loadExpressions = liftM fromRight . parseFromFile parseExpressions

parseExpression :: String -> Expression
parseExpression s = head $ fromRight $ parse parseExpressions "direct input" (s ++ "\n")

assert' :: Bool -> IO()
assert' x = assert x $ putStr "."

main :: IO()
main = do
  assert' (parseExpression "a" == parseExpression "((a))")
  assert' (parseExpression "a b c" == parseExpression "(a b) c")
  assert' (parseExpression "λ x . y z" == parseExpression "λ x . (y z)")
  assert' (parseExpression "λ x . (y) z" == parseExpression "λ x . (y z)")
  assert' (parseExpression "λ x . λ x . x" == parseExpression "λ x . (λ x . x)")
  assert' (parseExpression "λ x . y x" /= parseExpression "(λ x . y) x")
  assert' (parseExpression "λ x . λ y . λ z . x y z" == parseExpression "λ x . (λ y . (λ z . ((x y) z)))")
  putStrLn " tests"
  print =<< loadExpressions "expressions.txt"
