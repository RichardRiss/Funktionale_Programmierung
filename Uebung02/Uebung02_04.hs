module Uebung02_04 where
    import Prelude hiding (zipWith, concatMap, zip)
{-
#######################
4 Higher Order
    without fold
#######################
-}

    -- combines two lists by given function
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith f [] _ = []
    zipWith f _ [] = []
    zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


    -- use function on input and concat the result  
    -- concatMap (take 3) [[1..], [10,11], [100..]] = [1,2,3,10,11,100,101,102]
    concatMap :: (a -> [b]) -> [a] -> [b]
    concatMap _ [] = []
    concatMap f (x:xs) = f x ++ concatMap f xs


    

{-
#######################
4 Higher Order
    with filter
#######################
-}

    -- filter double entries
    nub :: [Int] -> [Int]
    nub [] = []
    nub (x:xs) = x : nub (filter (/=x) xs)
    

    -- remove from list by condition
    -- (.) :: (b -> c) -> (a -> b) -> a ->-> c
    removeIf :: (a -> Bool) -> [a] -> [a]
    -- removeIf f x = filter (not . f) x
    removeIf f = filter (not . f)


    -- split list into two by condititon
    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition f x = (filter f x, filter (not . f) x)



{-
#######################
4 Higher Order
    with map
#######################
-}

    -- inits but with map
    -- inits [1,2] = [[],[1],[1,2]]
    inits :: [a] -> [[a]]
    inits [] = [[]]
    inits (x:xs) = [] : map (x:) (inits xs)


{-
#######################
4 Higher Order
    with map and filter
#######################
-}

    -- find all Positions in List not just first
    -- lookupAll "two" [("one", 1), ("two", 2), ("three", 3),("two",2)]
    lookupAll :: String -> [(String, Int)] -> [Int]
    lookupAll x y = map (\(_,v) -> v) (filter(\(k,_) -> k == x) y)


{-
#######################
4 Higher Order
    with zip
#######################
-}
    zip :: [a] -> [b] -> [(a, b)]
    --zip (x:xs) (y:ys) = zipWith (\x y -> (x,y)) xs ys
    zip = zipWith(,) 