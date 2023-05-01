module Uebung02_07 where
    import Prelude hiding(length,(++),concat, lookup)
        
{-
#######################
7 Folds
#######################
-}

{-
1

foldr (:) []
concatenate

foldl (*) 1
multiply

foldr (-) 1
??

foldl (-) 1
substract
-}



{-
Implementations with foldr
-}

    -- length
    length :: [a] -> Int
    length = foldr(\_ acc -> acc + 1) 0


    (++) :: [a] -> [a] -> [a]
    (++) = foldr (:)

 
    concat :: [[a]] -> [a]
    --concat = foldr(\x acc -> acc ++ x) []
    concat = foldr(flip(++)) []


    lookup :: Eq a => a -> [(a, b)] -> Maybe b
    lookup x = foldr(\(k,v) acc -> if k == x then Just v else acc) Nothing
