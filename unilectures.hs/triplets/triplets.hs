{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import System.Environment (getArgs)
-- import System.IO.UTF8 (interact)

instance Monad (Either String) where  
    return = Right
    Left l  >>= _  = Left l
    Right x >>= f  = f x
    fail = Left


data Code a b = Code { encode :: [a] -> [b]
                     , decode :: [b] -> [a]
                     }

emptyC = Code id id

beforeC :: Code a b -> Code b c -> Code a c
beforeC (Code e1 d1) (Code e2 d2) = Code (e2.e1) (d1.d2)

flipC :: Code a b -> Code b a
flipC (Code e d) = Code d e

data Trit = A | B | C deriving Show

wavyC :: Code Trit Char
wavyC = Code (encode =<<) (map decode)
  where 
    encode A = "-"
    encode B = "+"
    encode C = "~"
    decode '-' = A
    decode '+' = B
    decode '~' = C

vVC :: Code Trit Char
vVC = Code (encode =<<) decode
  where 
    encode A = "v"
    encode B = "V"
    encode C = "\\/"
    decode [] = []
    decode ('v':xs) = A:decode xs
    decode ('V':xs) = B:decode xs
    decode ('\\':'/':xs) = C:decode xs
    decode s = error $ "Could not decode:"++show s

slashC :: Code Trit Char
slashC = Code (map encode) (map decode)
  where 
    encode A = '/'
    encode B = '|'
    encode C = '\\'
    decode '/' = A
    decode '|' = B
    decode '\\' = C

hashC :: Code Trit Char
hashC = Code (map encode) (map decode)
  where 
    encode A = '⌗'
    encode B = '♯'
    encode C = '⋕'
    decode '⌗' = A
    decode '♯' = B
    decode '⋕' = C
    decode s = error $ "Could not decode:"++show s

dPC :: Code Trit Char
dPC = Code (map encode) (map decode)
  where 
    encode A = 'd'
    encode B = 'b'
    encode C = 'q'
    decode 'd' = A
    decode 'b' = B
    decode 'q' = C

rnmC :: Code Trit Char
rnmC = Code (map encode) (map decode)
  where 
    encode A = 'r'
    encode B = 'n'
    encode C = 'm'
    decode 'r' = A
    decode 'n' = B
    decode 'm' = C

showC :: (Show a, Read a) => Code a Char
showC = Code show read

numsC :: Code Char Int
numsC = Code (map ord) (map chr)

tripletC :: Code Char Trit
tripletC = numsC `beforeC` tripletNumC

tripletNumC :: Code Int Trit
tripletNumC = Code (encodeOne =<<) decode
  where
    encodeOne :: Int -> [Trit]
    encodeOne x
      | x < 729   = A : padTo 6 (tritify x)
      | otherwise = B : padTo 6 (tritify (x `div` 2187)) ++ padTo 7 (tritify (x `mod` 2187))
      where
        tritify :: Int -> [Trit]
        tritify 0 = [A]
        tritify 1 = [B]
        tritify 2 = [C]
        tritify a = tritify (a `div` 3) ++ tritify (a `mod` 3)
        padTo n xs = if diff > 0 then pad xs diff else xs
          where diff = n - length xs
                pad = (!!) . iterate (A:)
    decode :: [Trit] -> [Int]
    decode [] = []
    decode (A:b:c:d:e:f:g:xs) = ( untritify.unpad $ [b, c, d, e, f, g] ) : decode xs
    decode (B:b:c:d:e:f:g:h:i:j:k:l:m:n:o:xs) = ( untritify.unpad $ [b, c, d, e, f, g, h, i, j, k, l, m, n, o]  ) : decode xs
    
    untritify = foldl (\n x -> n*3 + untritifyOne x) 0
        where
            untritifyOne A = 0
            untritifyOne B = 1
            untritifyOne C = 2
    unpad (A:xs) = unpad xs
    unpad x = x



getDigitCode :: String -> Either String (Code Trit Char)
getDigitCode "wavy" = return wavyC
getDigitCode "vV" = return vVC
getDigitCode "slash" = return slashC
getDigitCode "hash" = return hashC
getDigitCode "dP" = return dPC
getDigitCode "rnm" = return rnmC
getDigitCode x = fail $ "Invalid code name: "++x


data Opts = Opts { mode :: Code Char Char -> String -> String
                 , code :: Code Trit Char
                 , inCode :: Code Char Trit
                 }

withOpts :: (Opts -> IO ()) -> IO ()
withOpts io = do
  args <- getArgs
  case getOpts args (Opts encode wavyC tripletC) of
    Left e     -> putStrLn $ "Error: "++e
    Right opts -> io opts

getOpts :: [String] -> Opts -> Either String Opts
getOpts [] r = return r
getOpts ("-d":xs)  o = getOpts xs $ o {mode = decode}
getOpts ("+d":xs)  o = getOpts xs $ o {mode = encode}
getOpts ("-e":x:xs)o = do f <- getDigitCode x; getOpts xs $ o {code = f}
getOpts ("-r":x:xs)o = do f <- getDigitCode x; getOpts xs $ o {inCode = flipC f}
getOpts  (x:_) _ = fail $ "Unknown option: "++x


main = withOpts $ \(Opts mode code inCode) -> interact $ mode (inCode `beforeC` code)

