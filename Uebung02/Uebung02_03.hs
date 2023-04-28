module Uebung02_03 where


{-
#######################
3 Partial function application
#######################
-}

    -- inc which raises int by one
    inc :: Int -> Int
    inc = (+) 1 

    -- half of number
    half :: Int -> Int
    half = (`div` 2)


    -- first 5 chars of string
    prefix :: String -> String
    prefix = take 5


    -- returns Value Int or a default value
    fromMaybeInt :: Maybe Int -> Int
    fromMaybeInt = fromMaybe1 0

    --helper Function
    fromMaybe1 :: a -> Maybe a -> a
    fromMaybe1 x Nothing  = x
    fromMaybe1 _ (Just y) = y



{-
#######################
3 Lambda
#######################
-}

    -- 3 Lambda variations of
    -- mult :: Int -> Int -> Int -> Int
    -- mult x y z = x * y * z
    
    -- variation 1 (Currying)
    mult1 = \x y z -> x * y * z
    

    -- variation 2 (nested)
    mult2 = \x y z -> mult2' (mult2' x y) z
    mult2' =  \x y -> x * y

    -- variation 3
    --mult3 = (*) . (*) . id


    