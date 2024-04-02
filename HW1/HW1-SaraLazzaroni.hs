-- Esercizio 1.1: Dare la definizione di myTakeWhile e myDropWhile
myTakeWhile p (x:xs) 
    | p x = x : myTakeWhile p xs
    | otherwise = []
myTakeWhile p [] = []

myDropWhile p (x:xs)
    | p x = myDropWhile p xs
    | otherwise = (x:xs)
myDropWhile p [] = []

-- Esercizio 1.2: scrivere una funzione ricorsiva myRemoveDupsOrd che rimuove i duplicati da una lista ordinata xs di lunghezza n in tempo O(n).
myRemoveDupsOrd [] = []
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd (x:y:xs) 
    | x == y = myRemoveDupsOrd (x:xs)
    | otherwise = x : myRemoveDupsOrd (y:xs)
    
-- Esercizio 1.3: scrivere una funzione myRemoveDups che rimuove i duplicati da una qualsiasi lista xs di lunghezza n in tempo O(nlogn), preservando l’ordine originale delle prime occorrenze degli elementi rimasti
qSort [] = []
qSort (x:xs) = qSort smaller ++ [x] ++ qSort larger where 
    smaller = [a | a <- xs,  a <= x ]
    larger = [b | b <- xs,  b > x ]
    
myRemoveDupsOrdTuples [] = []
myRemoveDupsOrdTuples [x] = [x]
myRemoveDupsOrdTuples (x:y:xs) 
    | fst x == fst y = myRemoveDupsOrdTuples (x:xs)
    | otherwise = x : myRemoveDupsOrdTuples (y:xs)
    
myRemoveDups xs = map snd (qSort ( map (\(x,y)->(y,x)) (myRemoveDupsOrdTuples (qSort (zip xs [0..])))))


-- Esercizio 2.1: Definire il funzionale zipWith f xs ys senza decomporre liste, ma usando un’espressione che contenga zapp, f ed eventualmente xs e ys
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _ _ = []
zipWith' f xs ys = zapp (map f xs) ys

-- Esercizio 2.2: Abbiamo visto che zipWith e piu generale di zip. Tuttavia si pu`o definire zipWith f xs ys usando zip e un paio di altri funzionali visti nella Lezione 3.
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

-- Esercizio 2.3: Definire il funzionale map f xs senza decomporre xs, ma usando un’espressione che contenga foldr, f e xs. Fare lo stesso usando foldl
map' f = foldr (\x -> (:) (f x) ) []
map'' f = foldl (\acc x -> acc ++ [f x]) []

-- Esercizio 2.4: Argomentare brevemente sul perch ́e non sia possibile definire foldl e foldr usando map
-- Soluzione: non si può fare perchè foldr ha un tipo più generale di map in quanto prevede l'utilizzo di un accomulatore cosa che map non prevede.

-- Esercizio 3.1: Scrivere una funzione prefissi :: [a] → [[a]] che ritorna tutti i segmenti iniziali di una lista
prefissi [] = [[]]
prefissi xs@(x:txs) = []: map(x:) (prefissi txs)

suffissi [] = []
suffissi xs@(_:txs) = xs: suffissi txs

-- Esercizio 3.2: Senza preoccuparsi dell’efficienza, ma usando i funzionali prefissi, suffissi e altri funzionali dello standard Prelude, scrivere una funzione segSommaS :: (Num a) ⇒ [a] → a → [[a]] che data una lista numerica xs e un valore s restituisce tutti i segmenti (cio`e sottoliste di elementi consecutivi) di xs di somma s
segSommaS xs s = filter (\ys -> sum ys == s) (concat (map suffissi (prefissi xs)))

-- Esercizio 3.3: Scrivere una funzione sublSommaS :: (Num a) ⇒ [a] → a → [[a]] che data una lista numerica e un valore s restituisce tutte le sottoliste (anche di elementi non consecutivi) di somma s
sottoliste [] = [[]]
sottoliste (x:xs) = map (x:) ys ++ ys where ys = sottoliste xs
sublSommaS xs s = filter (\ys -> sum(ys) == s) (sottoliste xs)

-- Esercizio 4.1: Scrivere una funzione Haskell part :: Int → Integer, che calcola il numero di partizioni di un certo numero n.
partInternals 0 _ _ = 1
partInternals n j k
    | j < k = 0
    | j == k = 1
    | j > k = sum ( map (\i -> partInternals n (j - k) i) [k..n])
part n = partInternals n (n + 1) 1

-- Esercizio 4.2: Se invece considero diverse tra loro anche partizioni che differiscono solo per l’ordine, quante sono?
-- Soluzione: Se è rilevante l'ordine degli elementi all'interno delle partizioni, per n = 4 il numero di partizioni è 8
partsAux 0 = 1
partsAux n = sum [partsAux (n-x) | x <- [1..n]]

-- Esercizio 4.3: Scrivere poi una funzione Haskell parts :: Int → [[Int]]. Che calcola la lista delle partizioni di n. Ad esempio, parts 4 calcola la lista [[1,1,1,1], [1,1,2], [1,3], [2,2], [4]]
parts 0 = [[]]
parts 1 = [[1]]
parts n = [x : y | x <- [1..n], y <- parts (n-x), null y || x >= head y]

-- Esercizio 4.4: Ma scrivere part usando parts? E la complessita` è molto maggiore della part originaria?
-- Soluzione: No la complessità rimane la stessa, infatti entrambe le implementazioni sono in O(2^n) in quanto, nonostante length sia lazy, deve comunque arrivare fino ai casi base e risalire nella ricorsione senza valutare la formazione delle liste.
partWithParts n = length (parts n)


