module Uebung02_07 where
    import Prelude hiding(length,(++),concat, lookup,map)
        
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
foldr (-) 1 [1,2,3] = (1-(2-(3-1)))

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

{-
Implementations with foldl and foldr
-}

    -- foldr
    map1 :: (a -> b) -> [a] -> [b]
    map1 f = foldr(\xs acc -> f xs : acc) []


    -- foldl
    map2 :: (a -> b) -> [a] -> [b]
    map2 f = foldl(\acc xs -> [f xs] ++ acc) []

    
    -- foldr
    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 f = foldr(\xs acc -> if f xs then xs:acc else acc) []

    --foldl
    filter2 :: (a -> Bool) -> [a] -> [a]
    filter2 f = foldl(\acc xs -> if f xs then [xs] ++ acc else acc) []


    {-
    Implementation with either foldl or foldr
    -}

    concatMapr :: (a -> [b]) -> [a] -> [b]
    concatMapr f = foldr(\xs acc -> f xs ++ acc) [] 
    
    nubl :: [Int] -> [Int]
    nubl = foldl(\acc xs -> if xs `elem` acc then acc else [xs] ++ acc) []

    reversel :: [a] -> [a]
    reversel = foldl(\acc xs -> xs : acc) []

    unzipr :: [(a, b)] -> ([a], [b])
    unzipr = foldr(\xs (k,v) -> (fst xs : k, snd xs : v) ) ([],[])