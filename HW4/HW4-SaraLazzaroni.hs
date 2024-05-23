import Data.Char (toUpper)
import Control.Applicative

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

-- Stampo solo le lettere delle parole inserite
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

newtype ST s a = S { runState :: s -> (a, s) }

instance Functor (ST s) where
  fmap f (S g) = S (\s -> let (a, s') = g s in (f a, s'))
  
instance Applicative (ST s) where
  pure a = S (\s -> (a, s))
  stf <*> stg = S (\s -> let (h, s') = runST stf s; (a, s'') = runST stg s' in (h a, s''))

instance Monad (ST s) where
  return = pure
  (S g) >>= f = S (\s -> let (a, s') = g s; S h = f a in h s')

runST (S st) = st

evalState st s = fst (runST st s)

-- Inizialmente la soluzione dell'esercizio era stata implementata nel seguente modo:
--  0. lo stato rappresentava (sumPath, sumSubtree) del nodo val.
--  1. prendo lo stato corrente (sumPath, sumSubtree) con get e lo salvo in una variabile.
--  2. aggiorno il path andando a salvare (sumPath + val, sumSubtree) prima della ricorsione sinistra
--  3. faccio la ricorsione sinistra che ritorna la propria lista di nodi bilanciati bsx e mi salvo i 
--      valori dello stato aggiornato (sumPathSx, sumSubtreeSx)
--  4. setto il valore (sumPath + val, sumSubtree) prima di fare la ricorsione destra
--  5. faccio la ricorsione destra che ritorna la propria lista di nodi bilanciati bdx e prendo i valori 
--      dello stato aggiornato (sumPathDx, sumSubtreeDx)
--  6. stabilisco se questo nodo è bilanciato ed in caso affermativo ritrono val:bsx++bdx altrimenti bsx++bdx
-- Il problema con questa soluzione è che ci sono troppe dipendenze tra ciò che viene fatto prima della
-- ricorsione e quello che viene fatto dopo, cioè devono essere salvati troppi valori come sumSubtreeSx o path.
-- Queste dipendenze rappresentano un problema se si vuole implementare la soluzione con Applicative.
-- Mentre tutte le altre dipendeze posso essere risolte utilizzando dei calcoli, comme sommare e 
-- poi sottrare il valore val, l'unica dipendeza che non si può togliere è quella di sumSubtreeSx. Quindi
-- è necessario andare ad implementare un'altra soluzione meno intuitiva ma facilmente implementabile con
-- Applicative. 

-- Idea: Invece che salvarci nello stato la somma dei sottoalberi, possiamo usare una lista in cui salviamo
-- la somma dei sottoalberi che calcoliamo. Se mi trovo sul Nodo val sx dx, allora dopo aver concluso le 
-- ricorsioni sinistra e destra, i valori della somma dei sottoalberi sx e dx saranno proprio i primi due
-- elementi della lista. Quindi prendo questi primi due valori e li utilizzo per verificare se il nodo è
-- bilanciato, e lascio inviariata il resto della lista. In questo modo, non ci sono dipendenze tra ciò
-- che accade tra la ricorsione sinistra e destra ed è possibile implementare la soluzione con Applicative.

-- La funzione updatePath serve per aggiornare il valore del path prima di fare ricorsione sinistra e destra. 
-- Quindi updatePath restituirà come valore di ritorno il path corrente che sarà poi usato per decidere 
-- se il nodo è bilanciato o meno e aggiornerà lo stato corrente con n1+val.
updatePath val = S (\(n1, n2) -> (n1, (n1 + val, n2)))

-- La funzione append serve per andare ad aggiungere 0 in resta alla lista delle somme dei sottoalberi quando
-- mi trovo nel caso base. In questo caso non mi interessa il valore di ritorno.
append val = S (\(n1, n2) -> (n1, (n1, val : n2)))

-- La funzione updateSumSubtree restituisce come valore la somma del sumSubtreesx + sumSubtreedx + val in cui 
-- sumSubtreeSx e sumSubtreedx sono i primi due elementi della lista delle somme dei sottolaberi. Inoltre
-- aggiornato lo stato andando a sottrarre dal path val in modo da riportare il valore della somma del cammino
-- a quella corrente e aggiunge la somma totale del sottoalbero del nodo corrente (sumSubtreesx + 
-- sumSubtreedx + val) alla lista dove ho tolto i primi due elementi.
updateSumSubtree val = S (\(n1, n2) -> (val + sum (take 2 n2), (n1 - val, val + sum (take 2 n2) : (drop 2 n2))))

balancedNodesM b = evalState (balancedNodesMAux b) (0, [])
    where
        balancedNodesMAux Empty = do _ <- append 0
                                     return []
        balancedNodesMAux (Node val left right) = do n <- updatePath val
                                                     lres <- balancedNodesMAux left
                                                     rres <- balancedNodesMAux right
                                                     totSubSum <- updateSumSubtree val
                                                     return ((if n == totSubSum then (val:) else id) lres ++ rres)

balancedNodesA b = evalState (balancedNodesAAux b) (0, [])
    where
        balancedNodesAAux Empty = append 0 *> pure []
        balancedNodesAAux (Node val left right) = (\n lres rres totSubSum -> (if n == totSubSum then (val:) else id) lres ++ rres)
                                                  <$> updatePath val
                                                  <*> balancedNodesAAux left
                                                  <*> balancedNodesAAux right
                                                  <*> updateSumSubtree val

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
data NatBin = End | Zero NatBin | One NatBin
    deriving (Show, Eq)

instance Ord NatBin where
    a <= b = (a == b) || (fst $ minAux a b) where
        minAux (Zero a) End = (False, False)
        minAux (One a) End = (True, True)
        minAux End (Zero b) = (False, False)
        minAux End (One b) = (True, True)
        minAux (Zero End) (One End) = (True, True)
        minAux (One End) (Zero End) = (False, True)
        minAux (Zero End) (Zero End) = (False, False)
        minAux (One End) (One End) = (False, False)
        minAux (Zero a) (Zero b) = if flag then (ris, True) else (ris, False) where (ris, flag) = minAux a b
        minAux (One a) (One b) = if flag then (ris, True) else (ris, False) where (ris, flag) = minAux a b
        minAux (One a) (Zero b) = if flag then (ris, True) else (False, True) where (ris, flag) = minAux a b
        minAux (Zero a) (One b) = if flag then (ris, True) else (True, True) where (ris, flag) = minAux a b

data Expression = Value NatBin 
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Mod Expression Expression
                deriving (Show, Eq)

data Maybe' a = Just' a | DivByZero | NegativeNumber | Overflow
    deriving (Show, Eq)

instance Functor Maybe' where
    fmap f DivByZero = DivByZero
    fmap f NegativeNumber = NegativeNumber
    fmap f Overflow = Overflow
    fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
    pure x = Just' x
    DivByZero <*> _ = DivByZero
    NegativeNumber <*> _ = NegativeNumber
    Overflow <*> _ = Overflow
    (Just' g) <*> mx = fmap g mx

instance Monad Maybe' where
    DivByZero >>= _ = DivByZero
    NegativeNumber >>= _ = NegativeNumber
    Overflow >>= _ = Overflow
    (Just' x) >>= f = f x

zero = Zero (Zero (Zero (Zero (Zero (Zero (Zero (Zero End)))))))
uno = One (Zero (Zero (Zero (Zero (Zero (Zero (Zero End)))))))
due = Zero (One (Zero (Zero (Zero (Zero (Zero (Zero End)))))))
tre = One (One (Zero (Zero (Zero (Zero (Zero (Zero End)))))))
quattro = Zero (Zero (One (Zero (Zero (Zero (Zero (Zero End)))))))

troppo = One (One (One (One (One (One (One (One End)))))))

addNatBin :: NatBin -> NatBin -> Maybe' NatBin
addNatBin a b = addBits a b (Zero End)
  where
    addBits :: NatBin -> NatBin -> NatBin -> Maybe' NatBin
    addBits End End x = Just' x
    addBits (Zero End) (Zero End) x = Just' x
    addBits (Zero End) (One End) (Zero End) = Just' (One End)
    addBits (Zero End) (One End) (One End) = Overflow
    addBits (One End) (Zero End) (Zero End) = Just' (One End)
    addBits (One End) (Zero End) (One End) = Overflow
    addBits (One End) (One End) (Zero End) = Overflow
    addBits (One End) (One End) (One End) = Overflow
    addBits (Zero a) End (Zero End) = fmap Zero (addBits a End (Zero End))
    addBits (Zero a) End (One End) = fmap One (addBits a End (Zero End))
    addBits (One a) End (Zero End) = fmap One (addBits a End (Zero End))
    addBits (One a) End (One End) = fmap Zero (addBits a End (One End))
    addBits End (Zero b) (Zero End) = fmap Zero (addBits End b (Zero End))
    addBits End (Zero b) (One End) = fmap One (addBits End b (Zero End))
    addBits End (One b) (Zero End) = fmap One (addBits End b (Zero End))
    addBits End (One b) (One End) = fmap Zero (addBits End b (One End))
    addBits (Zero a) (Zero b) (Zero End) = fmap Zero (addBits a b (Zero End))
    addBits (Zero a) (Zero b) (One End) = fmap One (addBits a b (Zero End))
    addBits (Zero a) (One b) (Zero End) = fmap One (addBits a b (Zero End))
    addBits (Zero a) (One b) (One End) = fmap Zero (addBits a b (One End))
    addBits (One a) (Zero b) (Zero End) = fmap One (addBits a b (Zero End))
    addBits (One a) (Zero b) (One End) = fmap Zero (addBits a b (One End))
    addBits (One a) (One b) (Zero End) = fmap Zero (addBits a b (One End))
    addBits (One a) (One b) (One End) = fmap One (addBits a b (One End))


shift :: NatBin -> Int -> NatBin
shift a 0 = a
shift a i = Zero (shift a (i-1)) 

mulNumBit (One End) (One b) = One (mulNumBit (One End) b)
mulNumBit (Zero End) (Zero b) = Zero (mulNumBit (Zero End) b)
mulNumBit (One End) (Zero b) = Zero (mulNumBit (One End) b)
mulNumBit (Zero End) (One b) = Zero (mulNumBit (Zero End) b)
mulNumBit _ _ = End

multNatBin a b = multNatBinAux a b 0 where
    multNatBinAux a (Zero b) i = multNatBinAux a b (i+1)
    multNatBinAux a (One b) i = do
        shifted <- Just' (shift a i)
        partialProduct <- multNatBinAux a b (i+1)
        addNatBin shifted partialProduct
    multNatBinAux _ _ _ = Just' End

subNatBin a b 
    | a < b = NegativeNumber
    |otherwise = Just' (subBits a b (Zero End)) 
    where
        subBits End End x = End
        subBits (Zero a) End (Zero End) = Zero (subBits a End (Zero End))
        subBits (Zero a) End (One End) = One (subBits a End (One End))
        subBits (One a) End (Zero End) = One (subBits a End (Zero End))
        subBits (One a) End (One End) = Zero (subBits a End (Zero End))
        subBits End (Zero b) (Zero End) = Zero (subBits End b (Zero End))
        subBits End (Zero b) (One End) = One (subBits End b (One End))
        subBits End (One b) (Zero End) = One (subBits End b (One End))
        subBits End (One b) (One End) = Zero (subBits End b (One End)) 
        subBits (Zero a) (Zero b) (Zero End) = Zero (subBits a b (Zero End))
        subBits (Zero a) (Zero b) (One End) = One (subBits a b (One End))
        subBits (Zero a) (One b) (Zero End) = One (subBits a b (One End))
        subBits (Zero a) (One b) (One End) = Zero (subBits a b (One End))
        subBits (One a) (Zero b) (Zero End) = One (subBits a b (Zero End))
        subBits (One a) (Zero b) (One End) = Zero (subBits a b (Zero End))
        subBits (One a) (One b) (Zero End) = Zero (subBits a b (Zero End))
        subBits (One a) (One b) (One End) = One (subBits a b (One End))


divNatBin a b 
    | b == zero = DivByZero
    | otherwise = do
        ris <- divmodAux a b (zero, a)
        Just' (fst ris)

modNatBin a b
    | b == zero = DivByZero
    | otherwise = do
        ris <- divmodAux a b (zero, a)
        Just' (snd ris)

divmodAux a b (p, q)
    | a < b = Just' (p,q)
    | otherwise = do
        q' <- subNatBin q b
        p' <- addNatBin p uno
        divmodAux q' b (p', q')

isTooLong x = (len x) > 8 where
    len (Zero a) = 1 + (len a)
    len (One a) = 1 + (len a)
    len End = 0

eval (Value x) = if isTooLong x then Overflow else Just' x
eval (Div x y) = do u <- eval x
                    v <- eval y
                    divNatBin u v 
eval (Mod x y) = do u <- eval x
                    v <- eval y
                    modNatBin u v
eval (Sub x y) = do u <- eval x
                    v <- eval y
                    subNatBin u v
eval (Mul x y) = do u <- eval x
                    v <- eval y
                    multNatBin u v
eval (Add x y) = do u <- eval x
                    v <- eval y
                    addNatBin u v

main :: IO ()
main = do 
    --putStrLn $ show $ balancedNodesM (Node 1 (Node 7 (Node 5 (Node 1 Empty Empty) (Node 1 Empty (Node 1 Empty Empty))) Empty) (Node 3 (Node 2 (Node 1 Empty Empty) (Node 1 Empty Empty)) Empty))
    --print $ show $ eval (Value (Zero (One (Zero (Zero (Zero (Zero (Zero (Zero End)))))))))
    --print $ show $ addNatBin troppo troppo
    --print $ show $ eval (Mul (Value quattro) (Value due))
    --print $ show $ eval (Add (Value quattro) (Value due))
    --print $ show $ eval (Mul (Value quattro) (Value due))
    print $ show $ eval (Div (Value quattro) (Value due))