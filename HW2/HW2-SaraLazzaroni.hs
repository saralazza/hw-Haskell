import Data.List
--Esercizio 1.1: Definire una funzione Haskell che segue la seguente idea bottom-up per implementare l’algoritmo mergeSort: Data una lista xs, creare una lista di liste lunghe 1, ciascuna contenente un elemento di xs, poi fondere a due a due le liste ordinate (eventualmente lasciando inalterata l’ultima lista quando il numero delle liste `e dispari), finch`e non rimane un’unica lista ordinata.
listaDiListe :: [a] -> [[a]]
listaDiListe [] = []
listaDiListe (x:xs) = [x] : listaDiListe xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:txs) ys@(y:tys) 
    | x<=y = x : (merge txs ys)
    | otherwise =  y: (merge xs tys)

mergeCoppie :: Ord a => [[a]] -> [[a]]
mergeCoppie [] = []
mergeCoppie [xs] = [xs]
mergeCoppie (x:y:xs) = (merge x y) : mergeCoppie xs

mergeIterativo :: Ord a => [[a]] -> [a]
mergeIterativo [xs] = xs
mergeIterativo xs@(x:txs) = mergeIterativo(mergeCoppie xs)

mergeSort :: Ord a => [a] -> [a]
mergeSort = mergeIterativo . listaDiListe 

-- Esercizio 1.2: Accelerare la prima fase di questo algoritmo per trarre vantaggio da input “favorevoli”. La migliorıa dovrebbe assicurare un comportamento lineare in casi particolarmente fortunati.
listaDiListe' :: Ord a => [a] -> [[a]]
listaDiListe' xs = (snd xss) : (fst xss)where 
    xss = listaDiListeAux xs
    listaDiListeAux [x] = ([], [x])
    listaDiListeAux (x:xs)
        | x < head lxs = (xss, x : lxs)
        | otherwise = (lxs : xss, [x])
        where (xss, lxs) = listaDiListeAux xs
    
mergeSort' :: Ord a => [a] -> [a]
mergeSort' = mergeIterativo . listaDiListe'

-- Esercizio 2.1: Scrivere i funzionali mapBT, mapBT’, foldrBT, foldrBT’, foldlBT, e foldlBT’ che generalizzano agli alberi BinTree e BinTree’ gli analoghi funzio- nali map, foldr e foldl sulle liste. Riflettete accuratamente sui tipi che devono avere e su quali siano, di fatto, i principi di ricorsione sugli alberi binari.
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving (Show)
    
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a
    deriving (Show)

mapBT f Empty = Empty
mapBT f (Node x sx dx) = Node (f x) (mapBT f sx) (mapBT f dx)

mapBT' f (Leaf x) = Leaf (f x)
mapBT' f (Node' sx dx) = Node' (mapBT' f sx) (mapBT' f dx)

foldrBT f v Empty = v
foldrBT f v (Node x sx dx) = f x (foldrBT f v sx) (foldrBT f v dx)

-- La funzione f indica come devono essere associati i valori al ritorno dalla ricorsione. La funzione g indica come devono essere associati i valori nel caso base
foldrBT' f g v (Leaf x) = g x v
foldrBT' f g v (Node' sx dx) = f (foldrBT' f g v sx) (foldrBT' f g v dx)

foldlBT f v Empty = v
foldlBT f v (Node x sx dx) =  foldlBT f left dx where
    left = foldlBT f (f v x) sx

foldlBT' f g v (Leaf x) = g v x 
foldlBT' f g v (Node' sx dx) = foldlBT' f g ( f (foldlBT' f g v sx)) dx

-- Esercizio 2.2: Scrivere poi le seguenti funzioni usando foldrBT e foldrBT’ (cercare di ottenere algoritmi lineari nel numero dei nodi): (a) numero dei nodi di un albero binario; (b) altezza dell’albero (= lunghezza in numero di archi del piu` lungo cammino radice-foglia); (c) massimo indice di sbilanciamento (= massima differenza tra altezza sotto-albero destro/sinistro)

numNodi tree = foldrBT (\ x sx dx-> sx + dx +1) 0 tree

hAlbero tree = foldrBT (\ x sx dx -> (max sx dx) + 1) (-1) tree

sbilanciamentoAux x (hssx, hsdx) (hdsx, hddx) = (1 + max hssx hsdx, 1 + max hdsx hddx)
  
sbilanciamentoBT tree = abs(sx-dx) where 
    ris = foldrBT (sbilanciamentoAux) (-1,-1) tree
    sx = fst ris
    dx = snd ris

numNodi' tree = foldrBT' (\sx dx -> sx + dx + 1) (\x acc-> acc) 1 tree

hAlbero' tree = foldrBT' (\sx dx -> 1 + max sx dx) (\x acc->acc) 0 tree

sbilanciamentoAux' (hssx, hsdx) (hdsx, hddx) = (1 + max hssx hsdx, 1 + max hdsx hddx)

sbilanciamentoBT' tree = abs(sx-dx) where 
    ris = foldrBT' (sbilanciamentoAux') (\x acc-> acc) (0,0) tree
    sx = fst ris
    dx = snd ris

-- Esercizio 2.3: Gli alberi a branching illimitato si possono facilmente definire in Haskell come segue: data Tree a = R a [Tree a]. Come ai punti precedenti, scrivendo i funzionali mapT, foldrT e foldlT.
data Tree a = R a [Tree a] deriving (Show)

mapT f (R x xs) = R (f x) (map (mapT f) xs)

foldr' f acc [x] = x
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

foldrT :: (a -> b -> b) -> b -> (b -> b -> b) -> Tree a -> b
foldrT f b g (R x xs) = f x (foldr' g b (map (foldrT f b g) xs))

foldlT f v (R x xs) = foldl (\acc st -> foldlT f acc st) (f v x) xs

numNodiT tree = foldrT (\x acc -> acc +1) 0 (+) tree

hAlberoT tree = foldrT (\_ acc -> acc + 1) (-1) (\x y -> max x y) tree

sbilanciamentoAuxT (hmin, hmax) (amin, amax) = (min hmin amin, max hmax amax)

sbilanciamentoT tree = massimo - minimo where 
    ris = foldrT (\x (amin, amax) -> (1 + amin, 1 + amax)) (-1, -1) (sbilanciamentoAuxT) tree
    minimo = fst ris
    massimo = snd ris

-- Esercizio 3: Scrivere una funzione nodiEquilibrati :: Num a ⇒ BinTree a → [a] che preso in input un albero, restituisce la lista (eventualmente vuota) contenente tutti i valori nei nodi equilibrati. Valutare la complessita` della funzione.
-- Soluzione: La complessità di questa funzione è data T(n) = T(n-k-1) + T(k) + O(n) che è O(n logn)

cercaNodiEquilibrati _ Empty = ([], 0)
cercaNodiEquilibrati sum (Node x sx dx)
    | equilibrato sum (sommaSx + sommaDx + x) = (x : listaSx ++ listaDx, sommaSx + sommaDx + x)
    | otherwise = (listaSx ++ listaDx, sommaSx + sommaDx + x)
    where
        (listaSx, sommaSx) = cercaNodiEquilibrati (sum + x) sx
        (listaDx, sommaDx) = cercaNodiEquilibrati (sum + x) dx

equilibrato sum sumSottoAlbero = sum == sumSottoAlbero

nodiEquilibrati = fst . cercaNodiEquilibrati 0 

-- Esercizio 4: Scrivere una funzione Haskell listToABR :: Ord a ⇒ [a] → BinTree a che sistema i valori di una lista in un albero binario di ricerca. Determinare la complessita` della funzione e chiedersi se si tratta di una complessita` ottima rispetto al problema.
-- Soluzione: La complessità di questa funzione è rappresentata da questa equazione di ricorrenza T(n) = 2T(n/2) + O(n) che è O(n logn). Si secondo me questo è il costo computazionale ottimo rispetto al problema perchè la minima altezza che un albero può avere è O(logn), che viene ottenuta se l'albero è bilanciato. 
--              Il bilanciamento dell'albero viene realizzato andando a dividere la lista in due sottoliste di uguale dimensione.

-- Rimuovo i duplicati dalla lista perchè la definizione di ABR non prevede la presenza di elementi duplicati all'interno dell'albero
myRemoveDupsOrd [] = []
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd (x:y:xs) 
    | x == y = myRemoveDupsOrd (x:xs)
    | otherwise = x : myRemoveDupsOrd (y:xs)

listToABR :: Ord a => [a] -> BinTree a
listToABR xs = listToABR' (length l) l where
    l = myRemoveDupsOrd (sort xs)

listToABR' _ [] = Empty
listToABR' _ [x] = Node x Empty Empty
listToABR' n xs = Node x (listToABR' m sx) (listToABR' m dx) where
        m = div n 2
        (sx, x:dx) = splitAt m xs

-- Esercizio 5: La funzione scanr :: (a → b) → b → [a] → [b] puo` essere facilmente definita componendo map, foldr e tails: 
--      scanr f e = map(foldr f e) . tails 
-- Usare la definizione sopra come specifica per derivare una definizione efficiente (cio`e lineare nella lunghezza della lista) facendo manipolazioni algebriche.
-- Soluzione:
-- Dimostrazione 1: Vogliamo dimostrare la seguente proprietà 
--          foldr f e = head . scanr f e
-- 1- Caso []:
--    head . scanr f e [] = {def scanr}
--    head ( map (foldr f e) . tails []) = {def .}
--    head ( map (foldr f e) (tails [])) = {def tails}
--    head ( map (foldr f e) [[]]) = {def map}
--    head ( [foldr f e []] ) = {def foldr}
--    head ( [e] ) = {def head}
--    e = {def foldr}
--    foldr f e []
-- 2- Caso (x:xs):
--    head . scanr f e (x:xs) = {def scanr}
--    head ( map (foldr f e) . tails (x:xs)) = {def .}
--    head ( map (foldr f e) (tails (x:xs))) = {def tails}
--    head ( map (foldr f e) ((x:xs) : tails (x:xs))) = {def map}
--    head ((foldr f e (x:xs)) : map (foldr f e) tails (x:xs)) = {def head}
--    foldr f e (x:xs) 

-- Ora possiamo dimostrare la proprietà scanr f e = map(foldr f e) . tails
-- 1- Caso []:
--    scanr f e [] = {def scanr}
--    map(foldr f e) . tails [] = {def .}
--    map(foldr f e) (tails []) = {def tails}
--    map(foldr f e) [[]] = {def map}
--    [foldr f e []] = {def foldr}
--    [e]

-- 2- Caso (x:xs)
--    scanr f e (x:xs) = {def scanr}
--    map (foldr f e) . tails (x:xs) = {def .}
--    map (foldr f e) (tails (x:xs)) =  {def tails}
--    map (foldr f e) ((x:xs) : tails xs) = {def map}
--    foldr f e (x:xs) : map (foldr f e) (tails xs) = {def foldr}
--    f x (foldr f e xs) : map (foldr f e) (tails xs) = {def scanr}
--    f x (foldr f e xs) : scanr f e xs = {dim 1}
--    f x (head (scanr f e xs)) : scanr f e xs
myScanr f e [] = [e]
myScanr f e (x:xs) = f x (head ys): ys where
    ys = myScanr f e xs 


main :: IO ()
main = do
    let ris =  listToABR [10,3,2,1,4,6,2,11,5,7]
    print ris
