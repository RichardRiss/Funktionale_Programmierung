module Uebung04_01 where



{-
#######################
Combinatorfunctions
#######################
-}


    --1.

    \x y -> f (g x) y = (f . g) x y =  f . g


    \f g x -> g (f x) = (g . f) x = flip (f . g) x = flip (.)

    \x y -> f (g (h x y)) = f ((g . h) x y) = (f . g . h) x y = f . g . h


    -- 2.
    flip id

    flip = \f -> \x -> \y -> f y x
    ip = \x -> x

    -- ausformulieren
    flip id = (\f -> \x -> \y -> f y x) (\x -> x)

    -- einsetzen der id funktion
    flip id = \x -> \y -> (\x -> x) y x

    -- Eta Reduktion (x gebunden durch id Funktion) 
    flip id = \x -> \y -> y x



    (.) . (.)

    (.) = \f -> \g -> \x -> f (g x)
    (.) = \f -> \g -> \y -> f (g y)

    -- ausformulieren
    (.) . (.) = (\f -> \g -> \x -> f (g x)) (\f -> \g -> \y -> f (g y))


    -- einsetzen der rechten (.) funktion
    (.) . (.) = \g -> \x -> (\f -> \g -> \y -> f (g x)) (g y)

    -- (g y) einsetzen
    (.) . (.) = \g -> \x -> (\g -> \y -> (g y) (g x))




 