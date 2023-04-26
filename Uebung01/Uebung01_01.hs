module Uebung01_01 where

{-
Set λ as prompt symbol
:set prompt  "λ: "

Set Codepage to not fail on Unicode symbols (set outside ghci)
chcp.com 65001

-}


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
intSum3 :: Int -> Int
intSum3 = intSum3' 0
--                    InputNumber -> Accumulator -> result
    where   intSum3' :: Int -> Int -> Int
            intSum3' c 0  = c
            intSum3' c n  = intSum3' (c + n) (n - 1)
