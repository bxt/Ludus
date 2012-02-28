
-- import Control.Monad.Error
import Data.Char
import System.Environment (getArgs)


data Trit = A | B | C deriving Show

wavy A = "-"
wavy B = "+"
wavy C = "~"

vV A = "v"
vV B = "V"
vV C = "\\/"

slash A = "/"
slash B = "|"
slash C = "\\"

hash A = "?"
hash B = "?"
hash C = "?"

dP A = "d"
dP B = "b"
dP C = "q"

rnm A = "r"
rnm B = "n"
rnm C = "m"

getDigitEncodeing :: String -> Maybe (Trit -> [Char])
getDigitEncodeing "wavy" = Just wavy
getDigitEncodeing "vV" = Just vV
getDigitEncodeing "slash" = Just slash
getDigitEncodeing "hash" = Just hash
getDigitEncodeing "dP" = Just dP
getDigitEncodeing "rnm" = Just rnm
getDigitEncodeing _ = Nothing



encode :: String -> [Trit]
encode = (=<<) (encodeOne.ord)

encodeOne :: Int -> [Trit]
encodeOne x
  | x < 729   = A : padTo 6 (tritify x)
  | otherwise = B : padTo 6 (tritify (x `div` 2187)) ++ padTo 7 (tritify (x `mod` 2187))

tritify :: Int -> [Trit]
tritify 0 = [A]
tritify 1 = [B]
tritify 2 = [C]
tritify a = tritify (a `div` 3) ++ tritify (a `mod` 3)

padTo n xs = if diff > 0 then pad diff xs else xs
  where diff = n - length xs

pad n xs = iterate (A:) xs !! n




decode :: [Trit] -> String
decode [] = ""
decode (A:b:c:d:e:f:g:xs) = ( chr.untritify.unpad $ (b:c:d:e:f:g:[]) ) : decode xs
decode (B:b:c:d:e:f:g:h:i:j:k:l:m:n:xs) = ( chr.untritify.unpad $ (b:c:d:e:f:g:h:i:j:k:l:m:n:[])  ) : decode xs


untritify = untritify' 0
  where untritify' n [] = n
        untritify' n (x:xs) = untritify' (n*3+untritifyOne x) xs
        untritifyOne A = 0
        untritifyOne B = 1
        untritifyOne C = 2

unpad (A:xs) = unpad xs
unpad x = x

data Opts = Opts {doDecode :: Bool, theDigis :: Trit -> [Char]}

getOpts :: (Opts -> IO ()) -> IO ()
getOpts io = do
  args <- getArgs
  case getOpts' (Right defaultOpts) args of
    Left x -> putStrLn $ "Error: "++x
    Right opts -> io opts
  where defaultOpts = Opts False wavy

getOpts' :: Either String Opts -> [String] -> Either String Opts
getOpts' l@(Left _) _ = l
getOpts' r [] = r
getOpts' (Right (Opts _ o2)) ("-d":xs) = getOpts' (Right (Opts True o2)) xs
getOpts' (Right (Opts _ o2)) ("+d":xs) = getOpts' (Right (Opts False o2)) xs
getOpts' (Right (Opts o1 _)) ("-e":x:xs)= case getDigitEncodeing x of 
  Nothing -> Left $ "Invalid digit code: "++x
  (Just f)-> getOpts' (Right (Opts o1 f)) xs
getOpts'  _ (x:xs) = Left $ "Unknown option: "++x

main = do
  getOpts (\(Opts decode digtis ) -> interact $ if decode
    then id
    else (=<<) digtis . encode )



