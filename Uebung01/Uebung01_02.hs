module Uebung01_02 where

{-
#######################
2
#######################
-}
-- Factorial helper function
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- binomial coefficient
binom :: Int -> Int -> Int
binom n 0 = 0
binom n k = div (fac n) (fac k * fac(n - k))

-- pascals triangle
    -- row <= pos
    --  top => row=0, pos=0
    --       1
    --     1   1
    --   1   2   1
    -- 1   3   3   1


pascal :: Int -> Int -> Int
pascal r p
    | p <  0 || r < 0 ||  p > r   = 0 --to prevent out of bounce inputs
    | p == r || p == 0  = 1
    | otherwise =  pascal (r - 1) (p - 1) + pascal (r - 1) p 

