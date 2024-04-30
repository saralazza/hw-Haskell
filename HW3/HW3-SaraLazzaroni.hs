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

-- Esercizio 1D.1: Scrivere in Haskell il funzionale primRec che definisce la ricorsione primitiva
primRec h g 0 = g
primRec h g n = h (n-1) (primRec h g (n-1))

-- Esercizio 1D.2: Scrivere in Haskell il funzionale primRec′ che definisce la ricorsione primitiva, senza fare ricorsione sui naturali, ma usando l’iterazione e le coppie, cio usando il funzionale for visto a lezione
for f 0 = \x -> x
for f n = f . (for f (n-1))

primRec' h g n = snd ((for (\(x, y) -> (x + 1, h x y)) n) (0,g))

-- Esercizio 1D.3: Dedurne che in λ–calcolo si può facilmente definire la ricorsione primitiva usando i numerali di Church
-- Soluzione: E' possibile definirla facilmente con i numerali di Church in quanto essi simulano lo stesso comportamento del for andando a comporre la funzione tante volte

-- Esercizio 2D.1: Scrivere un one-liner Haskell partsFromAll tale che partsFromAll n allPartitions sia proprio la lista di liste che rappresenta le partizioni di n (in ordine ascendente, preferibilmente).

-- TODO: da modificare
partsFromAll n xss = (takeWhile (\xs -> n /= head xs) (map (\xs -> take (length (takeWhile (\x -> x < n) (scanl (+) 0 xs))) xs) xss)) ++ [[n]]

-- Esercizio 2D.2: Scrivere un’equazione ricorsiva che genera allPartitions.
newPartition xs = take (n-1) xs ++ [(xs !! (n-1)) + 1 ]where 
    n = length xs

discendenti [] = True
discendenti [x] = True
discendenti (x:y:xs)
    | x<y = False
    | otherwise = discendenti (y:xs)

allPartitions = (repeat 1) : allPartitionsAux 2 where
    allPartitionsAux n = map ( ++ repeat 1) (filter discendenti ( map newPartition (partsFromAll (n-1) allPartitions))) ++ allPartitionsAux (n+1)
    
-- Esercizio 2D.3: Sviluppare qualche idea per rappresentare altre strutture combinatorie in modo analogo, tipo: tutti i sottoinsiemi (finiti) dei Naturali.


-- Esercizio 3D.1: Date una definizione circolare dei numeri di Ulam, usando allSums ulams
allSums [] = []
allSums (x:xs) = map (x+) xs:allSums xs

count x [] = 0
count x (y:ys) 
    | x==y = 1 + count x ys
    | otherwise = count x ys

diags (xs:xss) = zipWith (:) xs ([]:diags xss)

ulams = 1:2: ps where
    ps = ulamNumbers 1 2
    ulamNumbers ndiag x = newUlam : ulamNumbers (ndiag + 1) newUlam where
        sums = allSums ulams
        interestedSums = filter (\y -> y > x) ( concat (take ndiag (diags sums)))
        filteredList = filter (\x -> (count x interestedSums) == 1 ) interestedSums
        newUlam = minimum filteredList

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
    let ris = take 22 (map (take 10) allPartitions)
    print ris

{-main :: IO ()
main = do
    let ris = take 100 ulams
    print ris-}

{-
main :: IO ()
main = do
    let ris = visitaLivelli (takeNlevels 4 calkinTree)
    print ris
-}

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
