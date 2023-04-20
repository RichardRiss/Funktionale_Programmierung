module Uebung01_03 where

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


-- approach square root for float values with deviation (babylonian method)
approachSqrt :: Float -> Float -> Float
approachSqrt n d = approachSqrt' n (n/2 + 1) d
    where approachSqrt' :: Float -> Float -> Float -> Float
          approachSqrt' n a d
            | abs(a - n/a) < d = a
            | otherwise = approachSqrt' n ((a + n/a) / 2) d
