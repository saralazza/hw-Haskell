import Data.Char (toUpper)
import Control.Monad.State

-- Esercizio 1: Definite un’azione charCount :: IO () che legge un numero n da tastiera, poi n stringhe e alla fine stampa il numero di stringhe in cui appare ciascuna lettera.
charCount :: IO ()
charCount = do
    putStrLn "Inserisci il numero di stringhe:"
    n <- readLn
    putStrLn "Inserisci le stringhe"
    strings <- getStrings n
    countAllChar <- countOccurrences strings
    putStrLn "Numero di occorrenze di ciascuna lettera:"
    putStrLn (countsToString countAllChar)

countsToString [] = ""
countsToString [(c, count)] = (c : ": " ++ show count)
countsToString ((c, count):rest) = (c : ": " ++ show count ++ "\n") ++ countsToString rest

count x [] = 0
count x (s:ss)
    | (elem x s) || (elem (toUpper x) s)= 1 + count x ss
    | otherwise = count x ss

countOccurrences strings = do
    occurrences <- mapM (\x -> return (x, count x strings)) ['a'..'z']
    return (filter (\(_, y) -> y /= 0) occurrences)

getStrings n 
    | n <= 0 = return []
    | otherwise = do
        string <- getLine
        strings <- getStrings (n-1)
        return (string : strings)


-- Esercizio 2: Un nodo u di un albero con xori numerici in tutti i nodi è detto equilibrato se
-- la somma delle chiavi nel cammino dalla radice a u (esclusa la chiave in u) è esattamente uguale 
-- alla somma delle chiavi del sotto-albero radicato in u (compresa la chiave in u).
-- Risolvere l’esercizio usando applicativi e monadi.

data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving (Show)

type NodeListState a = State (a, a) [a]

nodiEquilibratiAux :: (Num a, Eq a, Show a) => BinTree a -> NodeListState a
nodiEquilibratiAux Empty = return []
nodiEquilibratiAux (Node x sx dx) = do
    (currentCammino, currentSottoalbero) <- get 
    put (currentCammino + x, currentSottoalbero) 
    sxList <- nodiEquilibratiAux sx 
    sxSottoalbero <- gets snd 
    put (currentCammino + x, currentSottoalbero) 
    dxList <- nodiEquilibratiAux dx
    dxSottoalbero <- gets snd 
    put (currentCammino + x, sxSottoalbero + dxSottoalbero + x) 
    return (if currentCammino == (sxSottoalbero + dxSottoalbero + x) then x : sxList ++ dxList else sxList ++ dxList)

nodiEquilibrati tree = fst $ runState (nodiEquilibratiAux tree) (0,0)

exT = (Node 5
        (Node 3
            (Node 2 Empty Empty) 
            Empty
        )
        (Node 1
            (Node 4 Empty Empty)
            Empty
        )
    )

-- Esercizio 3: Definire il tipo NatBin che rappresenta i numeri naturali come sequenze binarie. Potete 
-- definirlo come liste (di lunghezza fissata) di 0 e 1, oppure potete dare una definizione con data 
-- (ad esempio usando 3 costruttori, di cui uno sia la costante 0 e gli altri due... in ogni caso, 
-- immaginare di definire una “parola di memoria”, quindi prevedete una lunghezza massima costante).
-- Definire un valutatore di espressioni aritmetiche su NatBin, analoghi a quelli visti a lezione, 
-- ma considerare tutte le operazioni aritmetiche (+, ×, div, mod e -). Estendere il tipo Maybe in modo 
-- che il risultato di un’espressione possa essere eventualmente un’eccezione diversa a seconda 
-- dell’eventuale situazione anomala che si è verificata: divisione per zero, numero negativo oppure 
-- overflow.
-- Potete completare l’esercizio facendo in modo che il tipo NatBin sia un’istanza delle usuali classi 
-- Eq, Ord, Num, Show.

-- sequenza binaria di 8 bit
data NatBin = NatBin Int Int Int Int Int Int Int Int
    deriving (Show, Eq, Ord)

data Bit = Zero | One deriving (Show)
data NatBin' = End | Bit Bit NatBin' deriving (Show)

data Expression = Value NatBin 
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Mod Expression Expression
                deriving (Show, Eq)

data Exception = DivByZero | NegativeNumber | Owerflow
    deriving (Show, Eq)

newtype ST s a = S (s -> (a, s))

app :: ST s a -> s -> (a, s)
app (S f) = f

instance Functor (ST s) where
    fmap f st = S (\s -> let (x, s') = app st s in (f x, s'))

instance Applicative (ST s) where
    pure x = S (\s -> (x, s))
    stf <*> stx = S (\s -> let (f, s') = app stf s in let (x, s'') = app stx s' in (f x, s''))

instance Monad (ST s) where
    stx >>= f = S (\s -> let (x, s') = app stx s in app (f x) s')
    return = pure

due' = NatBin 0 0 0 0 0 0 1 0

tre' = NatBin 0 0 0 0 0 0 1 1

quattro' = NatBin 0 0 0 0 0 1 0 0

-- prendo una lista di bit perchè potrei dover fare la somma tra tre bit
-- somma tra bits
sumBits :: [Int] -> Int
sumBits bits = mod (sum bits) 2

-- calcolo del riporto della somma tra bits (vale 1 se la somma è 2 o 3, vale 0 se la somma è 1 o 0)
carryBits :: [Int] -> Int
carryBits bits = div (sum bits) 2

halfAdder :: Int -> Int -> (Int, Int)
halfAdder a b = (sumBits [a,b], carryBits [a,b])

fullfAdder :: Int -> Int -> Int -> (Int, Int)
fullfAdder a b cin = (sumBits [a,b,cin], carryBits [a,b,cin])

-- somma tra due numeri binari: il primo bit lo faccio con un halfadder perchè non ha il riporto in ingresso
-- tutti gli altri li faccio con il fulladder perchè ha anche il riporto
addNatBinAux   (NatBin a7 a6 a5 a4 a3 a2 a1 a0) (NatBin b7 b6 b5 b4 b3 b2 b1 b0) = 
    NatBin s7 s6 s5 s4 s3 s2 s1 s0 where
        (s0, c0) = halfAdder a0 b0
        (s1, c1) = fullfAdder a1 b1 c0
        (s2, c2) = fullfAdder a2 b2 c1
        (s3, c3) = fullfAdder a3 b3 c2
        (s4, c4) = fullfAdder a4 b4 c3
        (s5, c5) = fullfAdder a5 b5 c4
        (s6, c6) = fullfAdder a6 b6 c5
        (s7, c7) = fullfAdder a7 b7 c6

-- negativo di un numero binario
negNatBin (NatBin a7 a6 a5 a4 a3 a2 a1 a0) = addNatBinAux  (NatBin (1-a7) (1-a6) (1-a5) (1-a4) (1-a3) (1-a2) (1-a1) (1-a0)) (NatBin 0 0 0 0 0 0 0 1)

subNatBin a b = addNatBinAux  a (negNatBin b)

mulNumBit a b
    | b == 0 = (NatBin 0 0 0 0 0 0 0 0)
    | otherwise = a

shift (NatBin a7 a6 a5 a4 a3 a2 a1 a0) i 
    | i == 0 = (NatBin a7 a6 a5 a4 a3 a2 a1 a0)
    | otherwise = (NatBin a6 a5 a4 a3 a2 a1 a0 0)

multNatBin (NatBin a7 a6 a5 a4 a3 a2 a1 a0) (NatBin b7 b6 b5 b4 b3 b2 b1 b0) =
    foldr (addNatBinAux) (NatBin 0 0 0 0 0 0 0 0) rows where 
        rows = map (\(n, bit) ->  shift (mulNumBit (NatBin a7 a6 a5 a4 a3 a2 a1 a0) bit) n ) (zip [0..] [b0,b1,b2,b3,b4,b5,b6,b7])

divNatBin :: Maybe NatBin -> Maybe NatBin -> Maybe NatBin
divNatBin mx my = do
  x <- mx
  y <- my
  if y == NatBin 0 0 0 0 0 0 0 0
    then Nothing
    else Just (fst $ divmodAux x y (NatBin 0 0 0 0 0 0 0 0, x))

modNatBin mx my = do
    x <- mx
    y <- my
    if y == NatBin 0 0 0 0 0 0 0 0
        then Nothing
        else Just( snd $ divmodAux x y ((NatBin 0 0 0 0 0 0 0 0), x))

divmodAux x y (p,q)
    | x < y = (p, q)
    | otherwise = divmodAux (subNatBin x y) y (addNatBinAux  p (NatBin 0 0 0 0 0 0 0 1), subNatBin x y)

logNatBin Nothing (Div x y) = S (\s -> (Nothing, Just DivByZero))
logNatBin Nothing (Mod x y) = S (\s -> (Nothing, Just DivByZero))
logNatBin v _ = S (\s -> (v, s))

eval (Value x) = S (\s -> (Just x, s))
eval (Div x y) = do u <- eval x
                    v <- eval y
                    logNatBin (divNatBin u v) (Div x y)
eval (Mod x y) = do u <- eval x
                    v <- eval y
                    logNatBin (modNatBin u v) (Div x y)

main :: IO ()
main = do
    print $ show $ let term = Mod (Value quattro') (Value (NatBin 0 0 0 0 0 0 0 0)) in app (eval term) Nothing