module Uebung03_01 where

{-
#######################
1 Functions as data structs
#######################
-}
type IntegerSet = Integer -> Bool


exampleSet :: IntegerSet
exampleSet = \ x -> elem x [4, 8, 15, 16, 23, 42]

exampleSet2 :: Integer -> Bool
exampleSet2 = \ x -> elem x [12, 13, 14, 15]


empty :: IntegerSet
empty x =  False


insert :: Integer -> IntegerSet -> IntegerSet 
insert x set = \y -> (y == x) || (set y)
{-
(insert 1 exampleSet) 1 -> True
-}


remove :: Integer -> IntegerSet -> IntegerSet
remove x set = \y -> set y && (y /= x)
{-
(remove 4 exampleSet) 4 -> False
-}

isElem :: Integer -> IntegerSet -> Bool 
isElem x set = set x


union :: IntegerSet -> IntegerSet -> IntegerSet
union set1 set2 = \y ->  set1 y || set2 y


intersection :: IntegerSet -> IntegerSet -> IntegerSet
intersection set1 set2 = \y -> set1 y && set2 y



difference :: IntegerSet -> IntegerSet -> IntegerSet
difference set1 set2 = \y -> set1 y && not (set2 y)


complement :: IntegerSet -> IntegerSet
complement set = \y -> not (set y)


{-
#######################
2 List to set/Set to List
#######################
-}

listToSet :: [Integer] -> IntegerSet 
listToSet xs = \y -> elem y xs

setToList :: IntegerSet -> [Integer]
{-
IntegerSet is defined as a function, because the set can potentially contain
an infinite number of Integers there is no way to implement this function
-}