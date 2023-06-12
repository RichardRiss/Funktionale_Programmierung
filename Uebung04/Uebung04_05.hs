module Uebung04_05 where
    


    {-
    #######################
    infinite lists
    #######################
    -}

    -- 1

    -- allCombinations :: [a] -> [[a]]
    -- take 10 (allCombinations [True,False])
    --[[],[True],[False],[True,True],[False,True],[True,False],[False,False],[True,True,True],[False,True,True],[True,False,True]]
    --allCombinations xs = concatMap sequence (iterate (xs :) [])

    allCombinations :: [a] -> [[a]]
    allCombinations xs = zs
        where zs = [] : [ (y:ys) | ys <- zs, y <- xs ]



    -- names :: [String]
    -- names = ["a".."z", "a1".."z1", "a2".."z2", ...]
    -- with list comprehension
    names :: [String]
    names = [y : x | x <- "" : [show n | n <- [1..]], y <- ['a'..'z']]


    -- without list comprehension
    names' :: [String]
    names' = concatMap (\x -> map ( : x) ['a'..'z']) ("" : [show n | n <- [1..]])