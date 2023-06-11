module Uebung04_03 where



    {-
    #######################
    type instance rose tree
    #######################
    -}


    data Rose a = Rose a [Rose a] deriving Show


    -- 1
    instance Eq a => Eq (Rose a) where
        (Rose i x) == (Rose j y) =    i == j && x == y

    instance Ord a => Ord (Rose a) where
        compare (Rose i x) (Rose j y) = compare i j <> compare x y

    

    -- 2

    class Pretty a where
        pretty :: a -> String

    
    instance Pretty a => Pretty (Rose a) where
        pretty = prettyTree 0
            where
                prettyTree :: Int -> Rose a -> String
                prettyTree indent (Rose i [])     = indentString indent i
                prettyTree indent (Rose i xs) = indentString indent i ++
                    --foldr (indentString (indent + 1)) (++) xs 
                    concatMap (prettyTree (indent + 1)) xs


                                                

                indentString n i
                    | n < 1     = pretty i ++ "\n"
                    | n == 1    = "+--" ++ pretty i ++ "\n"
                    | n > 1     = "|    " ++ indentString (n - 1) i



    instance Pretty Integer where
        pretty = show


    test = Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []]
