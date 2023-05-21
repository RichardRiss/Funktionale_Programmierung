module Uebung03_05 where

{-
#######################
1 Hamming numbers

    create an infinite list of nice numbers
    sort them
    prepend 1
#######################
-}

    nice :: [Integer]
    nice = 1 : conc (map (3*) nice) (conc (map (5*) nice) (map (7*) nice))
        where
            conc :: [Integer] -> [Integer] -> [Integer]
            conc (x:xs) (y:ys)
                 | x < y        = x : conc xs (y:ys)
                 | x > y        = y : conc (x:xs) ys
                 | otherwise    = x : conc xs ys 
            

