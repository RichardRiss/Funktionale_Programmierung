module Uebung02_05 where

{-
#######################
5 polymorph binary trees
#######################
-}


    data Tree a = Leaf a | Node (Tree a) (Tree a)
        deriving (Show, Eq)


    -- use function on elements in tree
    mapTree :: (a -> b) -> Tree a -> Tree b
    mapTree f (Leaf a) = Leaf(f a)
    mapTree f (Node l r) = Node (mapTree f l) (mapTree f r) 


    -- fold Tree to only one element
    -- f ->  map each leaf node 'a' to  'b'
    -- g -> combine mapped values 'b' to single 'b'
    foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
    foldTree f _ (Leaf i) = f i 
    foldTree f g (Node l r) = g (foldTree f g l) (foldTree f g r)

    -- foldTree id (+) exampleTree = 6 

    -- implement with foldTree, concat for Trees
    -- Leaf will be returned as Leafs, Subtrees as Nodes
    flatTree :: Tree (Tree a) -> Tree a
    --flatTree t = foldTree id Node t
    flatTree = foldTree id Node


    -- example Tree
    exampleTree :: Tree Int
    exampleTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

    exampleTree2 :: Tree(Tree Int)
    exampleTree2 = Node (Leaf (Leaf 1)) (Node (Leaf (Leaf 2)) (Leaf (Leaf 3)))

    exampleTree3 :: Tree(Tree Int)
    exampleTree3 = Node (Node (Leaf (Node (Leaf 40) (Leaf 100))) (Leaf (Leaf 40))) (Node (Leaf (Leaf 40)) (Leaf (Leaf 40)))