module Uebung01 where
import GHC.Base (VecElem(Int16ElemRep))
   -- import Prelude hiding (++)


{-
#######################
1
#######################
-}
-- sum of natural numbers with gaussian sum formula
intSum1 :: Int -> Int
intSum1 n = div(n^2 + n) 2


-- sum of natural numbers with recursion
intSum2 :: Int -> Int
intSum2 0 = 0
intSum2 n = n + intSum2(n - 1)


-- sum of natural numbers with accumulator
intSum3 :: Integer -> Integer
intSum3 n = intSum3' n 0
--                    InputNumber -> Accumulator -> result
    where   intSum3' :: Integer -> Integer -> Integer
            intSum3' 0 c = c
            intSum3' n c = intSum3' (n - 1) (c + n)



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
    | p == r    = 1
    | p == 0    = 1
    | p <  0    = 0
    | p >  r    = 0
    | otherwise =  pascal (r - 1) (p - 1) + pascal (r - 1) p 



{-
#######################
3
#######################
-}

-- integer square root (binary search)
intSqrt :: Int -> Int
intSqrt n
    | n <= 0 = 0
    | otherwise = approx 0 (n `div` 2 + 1)
        where approx :: Int -> Int -> Int
              approx min max
                | min > max = max
                | mid * mid <= n = approx (mid + 1) max
                | otherwise = approx min (mid - 1)
                where mid = (min + max) `div` 2 

    