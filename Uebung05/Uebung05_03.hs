module Uebung05_03 where


    {-
    ########################
    functional lists
    ########################
    -}

    newtype FList a = FList ([a] -> [a])

    instance Semigroup (FList a) where
        (<>) :: FList a -> FList a -> FList a
        FList xs <> FList ys = FList (xs . ys)

    instance Monoid (FList a) where
        mempty :: FList a
        mempty = toFList []


    toFList :: [a] -> FList a
    toFList xs = FList (xs ++)

    fromFList :: FList a -> [a]
    fromFList (FList fl) = fl []
        


    singleton :: a -> FList a
    singleton xs = toFList [xs]

    cons :: a -> FList a -> FList a
    cons x fl = toFList $ x: fromFList fl 


    snoc :: FList a -> a -> FList a
    snoc fl x = toFList $ fromFList fl ++ [x]

    append :: FList a -> FList a -> FList a
    append = (<>)

    concat' :: [FList a] -> FList a
    concat' [] = toFList []
    concat' (fl:fls) = append fl (concat' fls)


    head' :: FList a -> a
    head' fl = head $ fromFList fl

    tail' :: FList a -> FList a 
    tail' = toFList . tail . fromFList

    foldr' :: (a -> b -> b) -> b -> FList a -> b
    foldr' f i (FList fl) = foldr f i (fl []) 


    reverse' :: [a] -> [a]
    reverse' l = fromFList (rev l)
        where
            rev []     = mempty
            rev (x:xs) = rev xs `mappend` toFList [x]


    instance Show a => Show (FList a) where
        show (FList f) = show (f [])
