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

main :: IO ()
main = charCount