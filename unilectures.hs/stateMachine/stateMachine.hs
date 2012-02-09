
data StateMachine s i = SM s (s -> i -> s) (s -> Bool)


data State = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 deriving Show

ueberfuehrungsfunktion :: State -> Char -> State

ueberfuehrungsfunktion S0 'b' = S1
ueberfuehrungsfunktion S1 'e' = S2
ueberfuehrungsfunktion S2 'r' = S3
ueberfuehrungsfunktion S3 'n' = S4
ueberfuehrungsfunktion S4 'h' = S5
ueberfuehrungsfunktion S5 'a' = S6
ueberfuehrungsfunktion S6 'r' = S7
ueberfuehrungsfunktion S7 'd' = S8
ueberfuehrungsfunktion S8  _  = S8

ueberfuehrungsfunktion  _ 'b' = S1

ueberfuehrungsfunktion _   _  = S0

akzeptierend :: State -> Bool

akzeptierend S8 = True
akzeptierend  _ = False

mySM :: StateMachine State Char
mySM = SM S0 ueberfuehrungsfunktion akzeptierend

runSM :: StateMachine t t1 -> [t1] -> Bool
runSM (SM s f a) is = a $ foldl f s is



main = interact $ unlines . filter (runSM mySM) . lines
