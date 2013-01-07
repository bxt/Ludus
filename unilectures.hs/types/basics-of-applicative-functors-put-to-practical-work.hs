
import ApplicativeParsec
import Numeric (readHex) -- not in my base :(

p_query :: CharParser () [(String, Maybe String)]

p_query = pair `sepBy` char '&'
  where pair = liftA2 (,) (many safe)
                          (optional (char '=' *> many safe))
        safe = oneOf urlBaseChars
             <|> char '%' *> liftA2 diddle hexDigit hexDigit
             <|> ' ' <$ char '+'
        diddle a b = toEnum . fst . head . readHex $ [a,b]

urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

