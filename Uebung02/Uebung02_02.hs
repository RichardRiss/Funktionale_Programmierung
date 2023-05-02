module Uebung02_02 where


{-
#######################
2 Case
    Patter Matching: f1
    Case Matching:   f2
#######################
-}

    -- give back a or first given value 
    -- in case of Nothing
    fromMaybe1 :: a -> Maybe a -> a
    fromMaybe1 x Nothing  = x
    fromMaybe1 _ (Just y) = y

    
    fromMaybe2 :: a -> Maybe a -> a
    fromMaybe2 x y = case y of
        Nothing -> x
        Just y -> y





    -- List of Maybe values to list of Just values
    catMaybes1 :: [Maybe a] -> [a]
    catMaybes1 []             = [] 
    catMaybes1 ((Just x):xs)  = x : catMaybes1 xs
    catMaybes1 (Nothing : xs) = catMaybes1 xs
 

    catMaybes2 :: [Maybe a] -> [a]
    catMaybes2 x = case x of
        []            -> []
        (Nothing: rest) -> catMaybes2 rest
        (Just x: rest)  -> x : catMaybes2 rest








    -- standard value for Nothing or function applied to Maybe value
    maybe1 :: b -> (a -> b) -> Maybe a -> b
    maybe1 x f Nothing = x
    maybe1 x f (Just y) = f y

    maybe2 :: b -> (a -> b) -> Maybe a -> b
    maybe2 x f y = case y of
        Nothing     -> x
        Just y      -> f y





    -- list of all left values
    -- data Either a b = Left a | Right b
    lefts1 :: [Either a b] -> [a]
    lefts1 []               = []
    lefts1 (Left x: rest)   = x : lefts1 rest 
    lefts1 (Right _ : rest) = lefts1 rest


    lefts2 :: [Either a b] -> [a]
    lefts2 x = case x of
        []               -> []
        (Left x: rest)   -> x : lefts2 rest
        (Right _ : rest) -> lefts2 rest

    -- testl = [Left 42, Right "asd", Left 44, Left 48]




    -- Left to one list, Right to other List
    partitionEithers1 :: [Either a b] -> ([a], [b])
    partitionEithers1 []                 = ([],[])
    partitionEithers1 (Left  x:xs)       = (x:l, r)
        where (l,r) = partitionEithers1 xs
    partitionEithers1 (Right x:xs)       = (l, x:r) 
        where (l,r) = partitionEithers1 xs

       


    partitionEithers2 :: [Either a b] -> ([a], [b])
    partitionEithers2 x             = 
        case x of
            []                          -> ([],[])
            (Left x  : rest)            -> (x:l, r)
            (Right x : rest)            -> (l, x:r) 
            
            where 
                (l,r) = partitionEithers2 (tail x)



    --testl = [Left 1, Right 2, Left 2, Right 3]




    -- use function on either left or right value
    either1 :: (a -> c) -> (b -> c) -> Either a b -> c
    either1 f g (Left x) = f x
    either1 f g (Right x) = g x


    either2 :: (a -> c) -> (b -> c) -> Either a b -> c
    either2 f g x = case x of
        (Left y) -> f y
        (Right y) -> g y 
