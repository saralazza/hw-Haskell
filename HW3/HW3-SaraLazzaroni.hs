import Data.List (concatMap)

-- Esercizio 1: Scrivere una funzione Haskell che genera la lista infinita di caratteri insonnia = "1 sheep 2 sheep 3 sheep 4 sheep ...". Provare a scrivere un “one-liner”, cioè un programma che semplicemente compone opportunamente funzioni. Puo` essere utile la funzione show :: Show a => a => String che trasforma un elemento di qualsiasi tipo che implementa la classe Show in una stringa, cioè una lista di caratteri.
insonnia = concat( map (\n -> show n ++ " sheep ") [1..])

-- Esercizio 2: Definite in Haskell la lista infinita di liste finite tartaglia, tale che tartaglia!!n sia l’n-esima riga del triangolo di Tartaglia, e quindi tartaglia!!n!!k sia il coefficiente binomiale (n k).
tartAux xs = zipWith (+) (0:xs) (xs++[0])
tartaglia = [1]: map tartAux tartaglia

-- Esercizio 3: Scrivere una funzione Haskell che genera lo stream dei numeri fortunati.
-- Dalla lista dei numeri che rimangono, il primo numero è il nuovo lucky number e poi filtro sulla lista
-- rimanente andando a zippare gli elementi della lista con gli indici a partire da i e prendendo solo i
-- numeri che sono in una posizione non multiplo del nuovo lucky number. In particolare l'indice i viene usato
-- per tenere conto del numero di lucky number che sono stati già messi nella lista di output.
luckyAux xs = 1 : filterL 3 xs where
    filterL i (fs:xs) = fs : filterL (i + 1) ([x | (n, x) <- zip [i..] xs, rem n fs /= 0])
-- Elimino tutti i numeri pari per il 2
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
partsFromAll n xss = (takeWhile (\xs -> n /= head xs) (map (\xs -> take (length (takeWhile (< n) (scanl (+) 0 xs))) xs) xss)) ++ [[n]]

-- Esercizio 2D.2: Scrivere un’equazione ricorsiva che genera allPartitions.
newPartition xs = take (n-1) xs ++ [(xs !! (n-1)) + 1 ] where 
    n = length xs

discendenti [] = True
discendenti [x] = True
discendenti (x:y:xs)
    | x<y = False
    | otherwise = discendenti (y:xs)

-- Idea: se sono nel caso n, allora prendo tutte le partizioni già generate che hanno somma (n-1), da queste
-- ottengo la nuova partizione andando ad aggiugnere 1 all'ultimo valore di ogni lista con la funzione 
-- newPartition ma delle partizioin risultanti prendo solo quelle discendenti. Dopo aver calcolato tutte le 
-- nuove partizioni per n, aggiungo ad ognuna la lista infinita di 1 e poi faccio la chiamata ricorsiva per (n+1)
allPartitions = (repeat 1) : allPartitionsAux 2 where
    allPartitionsAux n = map ( ++ repeat 1) (filter discendenti (map newPartition (partsFromAll (n-1) allPartitions))) ++ allPartitionsAux (n+1)
    
-- Esercizio 2D.3: Sviluppare qualche idea per rappresentare altre strutture combinatorie in modo analogo, tipo: tutti i sottoinsiemi (finiti) dei Naturali e tutte le permutazioni (a dominio finito) dei Naturali.
-- Idea: se sono nel caso n, prendo tutti i sottoinsiemi già generati che iniziano con un valore minore di n
-- (devo farlo perchè allSubSet è uno stream infinito e devo usare takeWhile), a questi sottoinsiemi aggiungo
-- n in testa e poi faccio la chiamata ricorsiva per (n+1)
allSubSet = [] : (allSubSetAux 1) where
    allSubSetAux n = (map (n:) (takeWhile (\xs ->  (xs == []) || (n > head xs)) allSubSet)) ++ (allSubSetAux (n+1))

-- Idea: e sono nel caso n, prendo tutte le permutazioni già generate che iniziano con un valore minore di n
-- (devo farlo perchè allPermutations è uno stream infinito e devo usare takeWhile) e da ognuna di queste genero
-- tutte le nuove permutazioni possibili. Quindi data una permutazione già esistente, inserisco n in tutte le possibili
-- posizioni nella permutazione. Alla fine faccio la chiamata ricorsiva per (n+1)
nextPermutation x i ys =  y1 ++ [x] ++ y2 where
    (y1, y2) = splitAt i ys

nextPermutations x i ys = if i == (n+1) then [] else [nextPermutation x i ys] ++ nextPermutations x (i+1) ys where 
    n = length ys

allPermutations = [] : allPermutationsAux 1 where
    allPermutationsAux n = (concatMap (\xs -> nextPermutations n 0 xs) ((takeWhile (\xs ->  (xs == []) || (n > head xs)) allPermutations))) ++ (allPermutationsAux (n+1))

-- Esercizio 3D.1: Date una definizione circolare dei numeri di Ulam, usando allSums ulams
-- Idea: 
--      - ndiag rappresenta il numero di diagonali che devo andare a leggere per trovare il nuovo numero di ulams
--      - x è l'ultimo numero di ulam che ho preso
--      Per calcolare il nuovo numero di ulams calcolo tutte le possibili somme (con la funzione allSums) 
--      generate dai numeri di ulams già calcolati presenti nella lista ulams. Dalla matrice di tutte
--      le somme possibili prendo le prime ndiag diagonali (mi serve perchè ho uno stream infinito di
--      stream infinit) e le unisco in una lista. Da questa lista prendo solo i valori che compaiono una volta 
--      perchè significa che sono fatti in modo unico con la somma di due numeri di ulam precedentemente 
--      calcolati. Dalla lista filtrata prendo il minimo che sarà il nuovo numero di ulams.
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
-- I valori dei nodi dell'albero sono una coppia che rappresenta (m,n). Se mi trovo in un figlio sinistro
-- di qualche padre (isLeft = True), allora il valore della radice di questo albero sarà (m,m+n), altrimenti
-- (isLeft = False) sarà (m+n,m)
calkinTreeAux (m,n) isLeft = Node newRoot (calkinTreeAux newRoot True) (calkinTreeAux newRoot False)
    where
        newRoot = if isLeft then (m, m+n) else (m+n, n)

calkinTree = Node (1,1) (calkinTreeAux (1,1) True) (calkinTreeAux (1,1) False)

-- Esercizio 4D.2: scrivere la funzione takeNlevels::Int -> BinTree a -> BinTree a che taglia un albero (eventualmente infinito) ai primi n livelli; stipulando che takeNlevels 0 b torni sempre l’albero vuoto Empty e takeNlevels 1 (Node r lft rgt) torni Node r Empty Empty, etc.,
takeNlevels 0 _ = Empty
takeNlevels 1 (Node x sx dx) = Node x Empty Empty
takeNlevels n (Node x sx dx) = Node x (takeNlevels (n-1) sx) (takeNlevels (n-1) dx)

-- Esercizio 4D.3: scrivere una funzione visitaLivelli :: BinTree a -> [a] che produce la lista dei valori contenuti nei nodi di un albero (finito) nella sequenza ottenuta da una visita per livelli.
-- Idea: aggiungo i nodi di cui devo visitare i figli in una lista.
visitaAux [] = []
visitaAux (Empty : xs) = visitaAux xs
visitaAux ((Node val sx dx) : xs) = val : visitaAux (xs ++ [sx, dx]) 

visitaLivelli tree = visitaAux [tree]

main :: IO ()
main = do
    let ris = "HW-SaraLazzaroni (1983548)"
    print ris
    