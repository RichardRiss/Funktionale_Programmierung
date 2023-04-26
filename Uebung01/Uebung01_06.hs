module Uebung01_06 where
  
{-
#######################
6
Search trees
#######################
-}

-- Typ SearchTree with labels of Int
data SearchTree a = Empty
                  | Node (SearchTree a) a (SearchTree a)
  deriving (Eq, Ord)


-- overwrite Show for better visibility
{-
instance Show a => Show (SearchTree a) where
  show tree = show (toList tree) 

toList ::  SearchTree a -> [a]
toList Empty = []
toList (Node t1 i t2) = toList t1 ++ [i] ++ toList t2
-}
instance Show a => Show (SearchTree a) where
  show :: Show a => SearchTree a -> String
  show Empty = ""
  show (Node left x right) =
    showTree (Node left x right) ""
    where
      showTree Empty _ = ""
      showTree (Node left x right) prefix =
        showTree right (prefix ++ "        ") ++
        prefix ++ "+-- " ++ show x ++ "\n" ++
        showTree left (prefix ++ "        ")




-- insert Number into Tree
insert :: Ord a => SearchTree a -> a -> SearchTree a
insert Empty x = Node Empty x Empty
insert (Node t1 i t2) x
  | x == i = Node t1 i t2
  -- if smaller go left
  | x < i  = Node (insert t1 x) i t2
  -- if bigger go right
  | x > i  = Node t1 i (insert t2 x)


-- check if Element in Tree
isElem :: Ord a => SearchTree a -> a -> Bool
isElem Empty _ = False
isElem (Node t1 i t2) x
  | x == i = True
  | x <  i = isElem t1 x
  | x >  i = isElem t2 x 


-- delete Element from Tree

delete :: Ord a => SearchTree a -> a -> SearchTree a
delete Empty _ = Empty
delete (Node t1 i t2) x
  | x == i = deleteElem (Node t1 i t2)
  | x <  i = Node (delete t1 x) i t2
  | x >  i = Node t1 i (delete t2 x)

deleteElem :: Ord a => SearchTree a -> SearchTree a
deleteElem (Node Empty i Empty) = Empty
deleteElem (Node Empty i t2)    = t2
deleteElem (Node t1 i Empty)    = t1
deleteElem (Node t1 i t2)       = Node t1 v (delete t2 v)
  where v = getSuccessor t2

-- get smallest element from next biggest Succesor Node
getSuccessor :: Ord a => SearchTree a -> a
getSuccessor (Node Empty i _) = i
getSuccessor (Node t1 _ _) = getSuccessor t1



-- Example tree
{-
        5
      /   \
     3     7
    / \   / \
   2   4 6   8
-}

tree :: SearchTree Int
tree = Node 
         (Node 
            (Node Empty 2 Empty) 
            3 
            (Node Empty 4 Empty)) 
         5 
         (Node 
            (Node Empty 6 Empty) 
            7 
            (Node Empty 8 Empty))


