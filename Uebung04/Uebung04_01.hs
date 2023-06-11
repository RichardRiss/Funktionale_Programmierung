module Uebung04_01 where



{-
#######################
Combinatorfunctions
#######################
-}


    --1.

    \x y -> f (g x) y = (f . g) x y =  f . g = (.)


    \f g x -> g (f x) = (g . f) x = flip (f . g) x = flip (.)

    \x y -> f (g (h x y)) = f ((g . h) x y) = (f . g . h) x y = f . g . h


    -- 2.
    flip id

    flip = \f -> \x -> \y -> f y x
    ip = \x -> x

    -- ausformulieren
    flip id = \x y -> id y x

    -- einsetzen der id funktion
    \x y = y



    --(.) . (.)

    -- Nutzung der Funktionskomposition
    (f . g) x = f (g x)

    ((.).(.)) f g x y = (.) ((.)f g) x y
    = (f . g) x y
    \f g x y = (f . g) x y

    -- Eta Komposition
    \f g x = (f . g) x

    




 