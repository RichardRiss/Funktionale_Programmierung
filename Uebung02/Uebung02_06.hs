module Uebung02_06 where
        
{-
#######################
6 JSON
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


{--
[
    { 
        "name": "meier",
        "besuchte_kurse": ["Funktionale Programmierung", "Verteilte Systeme", "Klöppeln am Nachmittag"],
        "note": null,
        "zugelassen": true
    },
    { 
        "name": "schmidt",
        "besuchte_kurse": ["Verteilte Systeme", "Klöppeln am Nachmittag"],
        "note": 2.7,
        "zugelassen": false
    }
]
--}

testData :: JSON
testData = 
        JArray [JObject[
                ("name",JString "meier"),
                ("besuchte Kurse", JArray [JString "Funktionale Programmierung", JString "Verteilte Systeme", JString "Klöppeln am Nachmittag"]),
                ("note", JNull),
                ("zugelassen", JBool True)
        ],
        JObject [
                ("name", JString "schmidt"),
                ("besuchte kurse",  JArray [JString "Verteilte Systeme", JString "Klöppeln am Nachmittag"]),
                ("note", JFloat 2.7),
                ("zugelassen", JBool False)
        ]
        ]

-- (function that accumulates Json + accumulator) -> init value
-- of accumulator -> Json Struct -> accumulated value
foldJSON :: (a -> JSON -> a) -> a -> JSON -> a
foldJSON f acc  JNull = acc
foldJSON f acc (JBool b) = f acc (JBool b)
foldJSON f acc (JInt i) = f acc (JInt i)
foldJSON f acc (JFloat fl) = f acc (JFloat fl)
foldJSON f acc (JString s) = f acc (JString s)
foldJSON f acc (JArray (x:xs)) = foldArray f (f acc x) xs
foldJSON f acc (JObject p) = foldObjects f acc p 


-- foldJSON helper function
foldArray :: (a -> JSON -> a) -> a -> [JSON] -> a
foldArray f acc [] = acc
foldArray f acc (x:xs) = foldArray f (f acc x) xs

foldObjects :: (a -> JSON -> a) -> a -> [(String, JSON)] -> a
foldObjects f acc [] = acc
foldObjects f acc ((k,v): xs) = foldObjects f (foldJSON f acc v) xs 



{-
countStr :: Int -> JSON -> Int
countStr acc JNull = acc
countStr acc (JBool _) = acc
countStr acc (JInt _) = acc
countStr acc (JFloat _) = acc
countStr acc (JString _) = acc + 1
countStr acc (JArray xs) = foldJSON countStr acc (JArray xs)
countStr acc (JObject pairs) = foldr (\(_,v) acc -> countStr acc v) acc pairs
-}

-- foldJSON countStr 0 testData