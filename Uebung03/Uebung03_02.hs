module Uebung03_04 where

{-
#######################
1 class definition ToJSON
#######################
-}


    data JSON = JNull
        | JBool Bool
        | JInt Int
        | JFloat Float
        | JString String
            | JArray [JSON]
            | JObject [(String, JSON)]
        deriving Show

    
    data Tree a = Empty
        | Node (Tree a) a (Tree a)


    class ToJSON a where
        toJSON :: a -> JSON


{-
#######################
2 Instances of ToJSON
#######################
-}

    instance ToJSON Bool where
        toJSON = JBool

    
    instance ToJSON a => ToJSON [a] where
        toJSON = JArray . map toJSON
    
    {-
    instance ToJSON (a,b) where
        toJSON (a,b) = JObject [("fst", toJSON a), ("snd", toJSON b)]
    -}

    instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
        toJSON (a, b) = JObject [("fst", toJSON a), ("snd", toJSON b)]

    instance ToJSON a => ToJSON (Tree a) where
        toJSON Empty = JNull
        toJSON (Node l x r) = JObject [("leftTree", toJSON l), ("value", toJSON x), ("rightTree", toJSON r)]
        
    

