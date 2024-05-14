import Data.Char (toUpper)

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

main :: IO ()
main = charCount