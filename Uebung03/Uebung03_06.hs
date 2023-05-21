module Uebung03_06 where

{-
#######################
1 combine maxBTree and replace
#######################
-}



    data BTree a = Empty
             | Leaf a
             | Node (BTree a) a (BTree a)
        deriving Show


    {-
    maxBTree :: BTree Int -> Int liefert die größte Beschriftung in einem mit ganzen Zahlen beschrifteten Binärbaum. Bei einem leeren Baum kann einfach ein Fehler geworfen werden.
    replace :: BTree a -> b -> BTree b ersetzt alle Beschriftungen in einem Binärbaum durch die gegebene Beschriftung.
    -}

    replaceMaxRec :: BTree Int -> a -> (BTree a, Int)
    replaceMaxRec Empty _ = error "Tree is empty"
    replaceMaxRec (Leaf x) y = (Leaf y, x)
    replaceMaxRec (Node l x r) y =
        let (l', lMax) = replaceMaxRec l y
            (r', rMax) = replaceMaxRec r y
        in (Node l' y r', max x (max lMax rMax))



    testTree :: BTree Int
    testTree = Node (Node (Leaf 2) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 10))
    
    {-
    replaceMaxRec testTree 5
    -}



{-
#######################
2 use replaceMaxRec to implement replaceMax
#######################
-}

    replaceMax :: BTree Int -> BTree Int
    replaceMax t = fst(replaceMaxRec t (snd(replaceMaxRec t 0)))