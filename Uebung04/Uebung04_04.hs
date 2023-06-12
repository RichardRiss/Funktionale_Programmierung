module Uebung04_04 where
    import Data.Maybe (listToMaybe)
    


    {-
    #######################
    list comprehension
    #######################
    -}

    -- 1
    --[[3],[5],[7],[9],[11]]
    a = [x | x <- [3..11], odd x]

    --[(5,False),(20,True),(25,False)]
    b = [(x, even x) | x <- [5,20,25]] --stepgenerator: [1,3 .. 18]

    -- [Just 1,Just 9,Just 25]
    c = [Just (x^2) | x <- [1,3,5]]

    --[(1,5),(1,4),(1,3),(1,2),(2,5),(2,4),(2,3),(3,5),(3,4),(4,5)]
    d = [(x,y) | x <- [1..4], y <- [5,4..x+1]]




    -- 2
    -- map as list comprehension
    map' :: (a-> b) -> [a] -> [b]
    map' f xs = [f x | x <- xs]

    -- lookup as list comprehension
    lookup' :: Eq a => a -> [(a,b)] -> Maybe b
    lookup' k t = listToMaybe [ y | (x,y) <- t, x == k]


    -- filter as list comprehension
    filter' :: (a -> Bool) -> [a] -> [a]
    filter' f xs = [x | x <- xs, f x]


    -- 3
    --tripleLstGen 3
    -- [(1,1,1),(1,1,2),(1,1,3),(1,2,2),(1,2,3),(1,3,3),(2,2,2),(2,2,3),(2,3,3),(3,3,3)]
    tripleLstGen :: Int -> [(Int,Int,Int)]
    tripleLstGen n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n]]


    -- 4
    -- unwords :: [String] -> String as list comprehension
    unwords' :: [String] -> String
    unwords' xs = (init . concat) [x ++ " " | x <- xs]


