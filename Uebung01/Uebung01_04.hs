module Uebung01_04 where
{-
#######################
4
activate Benchmark with :set +s
#######################
-}


-- char at position x in List
charAt :: String -> Int -> Char
charAt s c
    | c+1 > length s = error "Exception! Index too large." 
    | otherwise = last (take (c+1) s)


-- rebuild take
initialString :: Int -> String -> String
initialString c s
    | length s <= c = s
    | otherwise = initialString c (init s) 


-- build Substring
--           String -> StartPos -> Length
subString :: String -> Int -> Int -> String
subString s n m = subString' n (take (n + m) s)
    where subString' :: Int -> String -> String
          subString' i ss
            | i == 0 = ss
            | otherwise = subString' (i - 1) (tail ss)



-- reverse list with O(n^2) 
-- reverse1 (replicate 10 'a') -> (0.01 secs, 69,984 bytes)
-- reverse1 (replicate 10000 'a') -> (1.86 secs, 4,303,962,264 bytes)
reverse1 :: [a] -> [a]
reverse1 []      = [] 
reverse1 (x:xs)  = reverse1 xs ++ [x]

-- reverse list with O(n)
-- use cons operator (:) to prepend to list
-- reverse2 (replicate 10 'a') -> (0.00 secs, 67,768 bytes)
-- reverse2 (replicate 10000 'a') -> (0.32 secs, 11,736,088 bytes)
reverse2 :: [a] -> [a]
reverse2 x = acc x []
    where acc :: [a] -> [a] -> [a]
          acc []     xss  = xss
          acc (x:xs) xss = acc xs (x:xss) 


-- index of first element in list of Int
-- Maybe a = Just a | Nothing
indexOf :: [Int] -> Int -> Maybe Int
indexOf x c = indexOf' x c 0
    where indexOf' :: [Int] -> Int -> Int -> Maybe Int
          indexOf' [] _ _ = Nothing
          indexOf' (x:xs) c i
            | x == c    = Just i
            | otherwise = indexOf' xs c (i+1) 


-- recoursive version of init and tail
-- inits:  [1,2] -> [[],[1],[1,2]]
-- tails:  [1,2] -> [[1,2][2],[]]
inits, tails :: [a] -> [[a]]
inits x = inits' [] x
    where inits' :: [a] -> [a] -> [[a]]
          inits' l  []     = [l]
          inits' l (x:xs)  = l : inits' (l ++ [x]) xs

tails x = tails' x 
    where tails' :: [a] -> [[a]]
          tails' []           = [[]]
          tails' (x:xs)       = (x:xs) :  tails' xs


-- new list with permutations of lists with inserted element
-- insert 1 [2,3] ->  [[1,2,3],[2,1,3],[2,3,1]]
insert :: a -> [a] -> [[a]]
insert i list = [insertAt i pos list | pos <- [0..length list]]

insertAt :: a -> Int -> [a] -> [a]
insertAt elem 0 x      = elem : x
insertAt elem i (x:xs) = x: insertAt elem (i-1) xs



-- give permutations of input list
-- take first element of list, insert it into permutations of remaining parts of the list
{-
permutations [] = [[]]
permutations xs = [ y : ps | (y,ys) <- selections xs, ps <- permutations ys]

selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]
-}

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = [zs | ys <- perms xs, zs <- insertAll x ys]

insertAll :: a -> [a] -> [[a]]
insertAll x []     = [[x]]
insertAll x (y:ys) = (x:y:ys) : map (y:) (insertAll x ys)
