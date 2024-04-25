-- Esercizio 1: Scrivere una funzione Haskell che genera la lista infinita di caratteri insonnia = "1 sheep 2 sheep 3 sheep 4 sheep ...". Provare a scrivere un “one-liner”, cioè un programma che semplicemente compone opportunamente funzioni. Puo` essere utile la funzione show :: Show a => a => String che trasforma un elemento di qualsiasi tipo che implementa la classe Show in una stringa, cioè una lista di caratteri.
insonnia = concat( map (\n -> show n ++ " sheep ") [1..])

-- Esercizio 2: Definite in Haskell la lista infinita di liste finite tartaglia, tale che tartaglia!!n sia l’n-esima riga del triangolo di Tartaglia, e quindi tartaglia!!n!!k sia il coefficiente binomiale (n k).

--tartaglia = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]
tartAux xs = zipWith (+) (0:xs) (xs++[0])
tartaglia = [1]: map tartAux tartaglia

-- Esercizio 3: Scrivere una funzione Haskell che genera lo stream dei numeri fortunati.
luckyAux xs = 1 : filterL 3 xs where
    filterL i (fs:xs) = fs : filterL (i + 1) ([x | (n, x) <- zip [i..] xs, rem n fs /= 0])

luckyNumbers = luckyAux (filter odd [3..])

-- Esercizio 4D.1: scrivere un’equazione ricorsiva che genera l’albero di Calkin-Wilf
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving (Show)

calkinTreeAux (m,n) isLeft = Node newRoot (calkinTreeAux newRoot True) (calkinTreeAux newRoot False)
    where
        newRoot = if isLeft then (m, m+n) else (m+n, n)

calkinTree = Node (1,1) (calkinTreeAux (1,1) True) (calkinTreeAux (1,1) False)

-- Esercizio 4D.2: scrivere la funzione takeNlevels::Int -> BinTree a -> BinTree a che taglia un albero (eventualmente infinito) ai primi n livelli; stipulando che takeNlevels 0 b torni sempre l’albero vuoto Empty e takeNlevels 1 (Node r lft rgt) torni Node r Empty Empty, etc.,
takeNlevels 0 _ = Empty
takeNlevels 1 (Node x sx dx) = Node x Empty Empty
takeNlevels n (Node x sx dx) = Node x (takeNlevels (n-1) sx) (takeNlevels (n-1) dx)

-- Esercizio 4D.3: scrivere una funzione visitaLivelli :: BinTree a -> [a] che produce la lista dei valori contenuti nei nodi di un albero (finito) nella sequenza ottenuta da una visita per livelli.
visitaAux [] = []
visitaAux (Empty : xs) = visitaAux xs
visitaAux ((Node val sx dx) : xs) = val : visitaAux (xs ++ [sx, dx]) 

visitaLivelli tree = visitaAux [tree]

main :: IO ()
main = do
    let ris = visitaLivelli (takeNlevels 4 calkinTree)
    print ris

{-
main :: IO ()
main = do
    let ris = take 57 luckyNumbers
    print ris-}

{-
main :: IO ()
main = do
    let ris = take 1000 insonnia
    print ris
-}

{-
main :: IO ()
main = do
    let ris =  tartaglia !! 0
    print ris
    let ris =  tartaglia !! 1
    print ris
    let ris =  tartaglia !! 2
    print ris
    let ris =  tartaglia !! 3
    print ris
    let ris =  tartaglia !! 4
    print ris
    let ris =  tartaglia !! 5
    print ris
    let ris =  tartaglia !! 6
    print ris
    let ris =  tartaglia !! 7
    print ris
    let ris =  tartaglia !! 4 !! 2
    print ris
-}
