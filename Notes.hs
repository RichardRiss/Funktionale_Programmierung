module Notes where
import Prelude hiding (double)

-- ghci
-- :l Notes
-- :r  


-- overwrite build-in func
double x = x + x 

-- overwrite with condition
doubleif x = if x < 100
            then x + x
            else x^2

-- Lists
-- add to end
  -- [1,2,3] ++ [4]
-- add to start
  -- 5:[1,2,3]
-- slice
 -- "Panda":0


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0
fib     :: Integer -> Integer
fib x
  | x < 0 = error "Illegal argument"
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = fib (x-2) + fib (x-1)


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit
fib2    :: Integer -> Integer
fib2 x
  | x < 0 = error "Illegal argument"
  | otherwise = fib2' 0 1 x
  where
    fib2'   :: Integer -> Integer -> Integer -> Integer
    fib2' n0 n1 x
      | x == 0 = n0
      | x > 0 = fib2' n1 (n0+n1) (x-1)