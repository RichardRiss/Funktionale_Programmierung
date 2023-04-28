module Uebung02_01 where

{-
#######################
1 Guards
#######################
-}

    -- Recursion steps to reduce a natural number n >= 1 to 1
    -- error if n < 1
    -- even n -> n/2
    -- odd n -> n * 3 + 1
    collatz :: Integer -> Integer
    collatz n
        | n < 1 = error "Input >= 1 required."
        | n == 1 = 0
        | even n =  1 + collatz (div n 2)
        | odd n = 1 + collatz (n * 3 + 1) 


    -- Fibnoacci with error for negative input
    fib :: Int -> Int
    fib n | n < 0  = error "No negative input allowed"
          | n < 2 = n
          | otherwise = fib (n-1) + fib (n-2)
