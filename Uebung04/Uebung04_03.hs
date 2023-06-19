module Uebung04_03 where



    {-
    #######################
    type instance rose tree
    #######################
    -}


    data Rose a = Rose a [Rose a] deriving Show


    -- 1
    instance Eq a => Eq (Rose a) where
        (==) :: Eq a => Rose a -> Rose a -> Bool
        (==) (Rose i x) (Rose j y) = i == j && x == y

    instance Ord a => Ord (Rose a) where
        (<=) :: Ord a => Rose a -> Rose a -> Bool
        (<=) (Rose i x) (Rose j y) = i <= j && x <= y

    

    -- 2

    class Pretty a where
        pretty :: a -> String

    
    instance Pretty a => Pretty (Rose a) where
        pretty = prettyTree 0
            where
                prettyTree :: Int -> Rose a -> String
                prettyTree indent (Rose i [])     = iString indent i
                prettyTree indent (Rose i xs)     = iString indent i ++ concatMap (prettyTree (indent + 1)) xs

                                            
                iString n i
                    | n < 1     = pretty i ++ "\n"
                    | n == 1    = "+--" ++ pretty i ++ "\n"
                    | n > 1     = "|    " ++ iString (n - 1) i



    instance Pretty Integer where
        pretty = show


    test = Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []]
