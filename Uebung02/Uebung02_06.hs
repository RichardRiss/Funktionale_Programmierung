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

foldJSON :: (a -> JSON -> a) -> a -> JSON -> a
