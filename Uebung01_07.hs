module Uebung01_07 where


{-
#######################
7
binary trees
#######################
-}

-- Create polymorph algebraic datatype Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show,Eq)

-- recursive function for sum of leaves in tree
sumTree1 :: Tree Int -> Int
sumTree1 (Leaf i) = i
sumTree1 (Node t1 t2) = sumTree1 t1 + sumTree1 t2

-- accumulator function for sum of leaves in tree
sumTree2 :: Tree Int -> Int
sumTree2 tree = sumTree2' tree 0
    where sumTree2' :: Tree Int -> Int -> Int
          sumTree2' (Leaf i) acc     = acc + i
          sumTree2' (Node t1 t2) acc = sumTree2' t1 (sumTree2' t2 acc)


-- mirror the tree on vertical axis
mirrorTree :: Tree a -> Tree a
mirrorTree (Leaf i) = Leaf i
mirrorTree (Node t1 t2) = Node (mirrorTree t2) (mirrorTree t1)

-- function to write all leaf labels to a list
toList :: Tree a -> [a]
toList (Leaf i) = [i]
toList (Node t1 t2) = toList t1 ++ toList t2


-- example Tree
exampleTree :: Tree Int
exampleTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))