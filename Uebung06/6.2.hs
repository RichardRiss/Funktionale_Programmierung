import Control.Monad (MonadPlus (..)) 


{-

Definiert eine Funktion permutations :: MonadPlus mp => [a] -> mp [a], die alle Permutationen einer übergebenen Liste berechnet. 
Wenn ihr die Nichtdeterminismussicht hierbei verwenden wollt, kann es sinnvoll sein, zunächst eine Hilfsfunktion zu definieren, 
welche ein Element nichtdeterministisch an jede beliebe Position einer Liste einfügt.

permutations :: MonadPlus mp => [a] -> mp [a]
permutations = undefined

insert :: MonadPlus mp => e -> [e] -> mp [e]
insert = undefined
Stell euch für mp, also vor, dass es hier Nichtdeterminismus gibt und ihr mplus nutzt um zwei mögliche Lösungen zu kombinieren.

Überlegt euch fürs Testen für welche Datentypen eine Instanzen für MonadPlus definiert ist.
        
-}

permutations :: MonadPlus m => [a] -> m [a]
permutations [] = return []
permutations (x:xs) = do
    ys <- permutations xs
    insert x ys


insert :: MonadPlus m => e -> [e] -> m [e]
insert x xs = monadConcat [ return(front ++ [x] ++ back) | i <- [0..length xs], let front = take i xs, let back = drop i xs ]

monadConcat :: MonadPlus m => [m a] -> m a
monadConcat [] = mzero
monadConcat (x:xs) = x `mplus` monadConcat xs
--monadConcat = foldr mplus mzero
-- Alternativ mit msum aus Control.Monad




{-
Definiert weiter eine Funktion solve, so dass solve l s alle Listen der Länge l von natürlichen Zahlen berechnet, die sich zu s summieren lassen:
solve 2 2
[[2,0],[1,1],[0,2]]
-}

solve :: MonadPlus mp => Int -> Int -> mp [Int]
solve 0 s
    | s == 0    = return []
    | otherwise = mzero
solve l s = monadConcat [return (x:xs) | x <- [0..s], xs <- solve (l-1) (s-x)]
 

-- Test
test :: IO ()
test = do
    print(permutations [1,2,3] :: [[Int]])
    print(solve 2 3 ::[[Int]])








    
    
    
 