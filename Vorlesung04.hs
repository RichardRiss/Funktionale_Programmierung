module Vorlesung04 where
    import Prelude hiding (Functor)



    class Moniod m where
        
        mempty :: m
        mappend :: m -> m -> m

    
    instance Moniod Bool where
        mempty = True
        mappend = (&&)

    instance Moniod Integer where
        mempty = 0
        mappend = (+)



    {-
    Anwendung:
    -----------
        1. Phase: Benutzerin gibt Daten frei
        2. Phase: Benutzerin fragt Daten an
    
    
    Funktioniert nicht nacheinander wegen Laziness

    putStr :: String -> IO ()

    putStrLn :: String -> IO ()

    print :: Show a => a -> IO ()

    nl :: IO ()

    (>>) :: IO () -> IO () -> IO ()
    (>>=) :: IO a -> (a -> IO b) -> IO b 


    IO-Aktionen sind Werte. Diese werden zu einer Gesamtaktion zusammengesetzt,
    welcher ausgeführt wird, wenn sie ganz oben (main) ankommt.
    
    -}

    main = let ac1 = putStr "Hallo " in
           ac1 >>
           putStrLn "Welt" >>
           ac1 >>
           print 42

    main1 = let ac1 = [putStr "Hallo ", putStrLn "Welt",
                        putStr "Juhu", print 42] in
            ac1 !! 0 >>
            ac1 !! 1 >>
            ac1 !! 1 >>
            ac1 !! 3


    {-
    getLine >> print "Hallo"
    Hi
    Hallo

    getLine >>= putStrLn
    hi
    hi
    -}

    main2 = getLine >>= \t1 ->
            putStr "Your input: " >> 
            putStrLn t1 >>
            getLine >>= \t2 ->
                (if t1 == t2 then
                    putStrLn "gleich"
                else
                    putStrLn "ungleich"
                    ) >>
                    print(t1,t2)

    

    -- getChar :: IO Char
    getLine' :: IO String
    getLine' = getChar >>= \c ->
        if c == '\n' then
            return ""
        else
            getLine' >>= \cs ->
                return (c:cs)


    main3 = do
        t1 <- getLine
        putStr "Input: " >> putStrLn t1
        t2 <- getLine
        if t1 == t2 then 
            return "gleich"
        else
            return "ungleich"

        


    -- List Comprehension

    l = [(x,y) | x <- [1..5], y <- [3..6], x /= y]

    map' :: (a-> b) -> [a] -> [b]
    map' f xs = [f x | x <- xs]

    data Tree a = Leaf a | Branch (Tree a) (Tree a)

    mapTree :: (a -> b) -> Tree a -> Tree b
    mapTree f (Leaf x) = Leaf (f x)
    mapTree f (Branch l r) = Branch (mapTree f l) (mapTree f r)

    -- fasse alle Datentypen zusammen für die es eine map-Funktion gibt

    class Functor f where
        -- Functor ist keine Klasse sondern eine Typkonstruktorklasse
        -- D.h. Instanzen sind keine Typen, sonder Typkonstruktoren mit Stelligkeit 1
        -- Maybe ist ein Typkontruktor -> Maybe a ist ein Typ

        fmap :: (a->b) -> f a -> f b

        -- Typ kann man sich mit :k ausgeben lassen (:k Maybe = * -> *)

        -- Typen: Int, Bool
        
        -- Typkonstruktoren mit Stelligkeit 1:
        -- [], Maybe, Tree hat alles Kind * -> *
        
        -- Typkonstruktoren mit Stelligkeit 2:
        -- Either, (->), (,), mit Kind * -> * -> *
        
        -- Either Int ist eine partielle Applikation der zweistelligen
        -- Typkonstruktors Either auf den Typ Int und hat Kind * -> *
        
        -- Typklassen abstrahieren über Typen z.B. Eq, Or, Num, Show
        
        -- Typkonstruktorklassen abstrahieren über Typkonstruktoren einer
        -- bestimmten Stelligkeit (oder partielle Applikation höherer Stelligkeit)
        -- Bsp. Functor 

    
    instance Functor Tree where

        fmap = mapTree
    
    instance Functor [] where
        fmap = map


    instance Functor Maybe where
        fmap f Nothing = Nothing
        fmap f (Just x) = Just (f x)

    instance Functor (Either a) where
        fmap f (Left x) = Left x
        fmap f (Right y) = Right (f y)    


    


