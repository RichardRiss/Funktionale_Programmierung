module Vorlesung02 where
    import Prelude hiding(map, flip, filter, sum, foldr,foldl, id)

    -- Funktion höhrer Ordnung = Funktion die Funktion als Parameter nimmt oder Funktion zurückgibt

    -- map (+1) [1,2,3,4] = [2,3,4,5]
    -- map even [1..10]
    map :: (a -> b) -> [a] -> [b]
    map f (x:xs) = f x : map f xs

    -- add :: Int -> Int -> Int "partielle Applikation" 
    add :: Int -> Int -> Int
    add x y = x + y

    -- partielle Applikation von map mit Typ [Int] -> [Int]
    func = map (add 1)
    -- func [1,2,3,4] = [2,3,4,5]
    -- func [[1,2],[3,4]] = [[2,3],[4,5]]
    funcPart = (`map` [1,2,3])

    -- Case
    map1 = \f xs -> case xs of
        [] -> []
        (x:xs) -> f x : map1 f xs
    -- map1 (+1) [1..10] = [2,3,4,5,6,7,8,9,10,11]

    

    -- Lambda
    -- let fun \x -> x + 1
    -- let fun \x y -> x + y

    flip :: (a -> b -> c) -> b -> a -> c
    flip f x y = f y x
    -- flip (-) 2 1 = -1

    -- curry/uncurry
    f = map (uncurry (+)) --[(1,2),(3,4),(5,6)] 
    -- = [3,7,11]

    filter :: (a->Bool) -> [a] -> [a]
    filter p [] = []
    filter p (x:xs) = if p x then x : filter p xs else filter p xs
    -- filter even [1..10] = [2,4,6,8,10]

    -- sum :: [Int] -> Int
    -- sum [] = 0
    -- sum (x:xs) = x + sum xs
    
    sum xs = foldr (+) 0 xs

    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f e [] = e
    foldr f e (x:xs) = f x (foldr f e xs)

    --map2 f xs = foldr (\x res -> f x : res) [] xs
    map2 f = foldr (\x -> (f x :)) []
    
    filter1 :: (a->Bool ) -> [a] -> [a]
    --filter1 p = foldr(\x res -> if p x then x:res else res) []
    filter1 p = foldr(\x -> if p x then (x:) else id) []
    
    -- id = Identitätsfunktion
    id :: a -> a
    id x = x


    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl f e [] = e
    foldl f e (x:xs) = foldl f (f e x) xs

    reverseL = foldl (flip (:)) []
    --reverseL = foldl (\res x -> x : res) []


    data Tree a = Empty | Node (Tree a) a (Tree a)
        deriving Show

    --Empty :: Tree a
    -- e :: b
    -- Node :: Tree a -> a -> Tree a -> Tree a
    -- f :: b -> a -> b -> b
    foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
    foldTree e n (Node tl x tr) = n (foldTree e n tl) x (foldTree e n tr)

    sumTree = foldTree 0 (\resl x resr -> resl + x + resr)
