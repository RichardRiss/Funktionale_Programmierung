{-
Für beliebige Monaden kann man eine Applicative Instanz wie folgt angeben:

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) ff fx = ff >>= \f -> fx >>= \x -> return (f x)

pure :: Monad m => a -> m a
pure = return
Beweist, dass diese Implementierung die Gesetze für applikative Funktoren erfüllt:

-}

--Identity: pure id <*> v = v
pure id <*> v =
    return id <*> v =
        return id >>= \f -> v >>= \x -> return (f x)

--(>>=) :: Monad m => m a -> (a -> m b) -> m b
-- Anwenden der Funktion \f -> v >>= \x -> return (f x) auf id (id bind to f)
-- ID extrahiert das v quasi aus der Monade und wendet es auf f an
v >>= \x -> return (id x)
-- id x = x
v >>= \x -> return x  = -- entspricht einfach der Identitätsfunktion von v
    v


-- Homomorphism: pure f <*> pure y = pure (f y)
return f <*> return y = return (f y)
return f >>= \g -> return y >>= \x -> return (g x) =
-- f und g einsetzen
return f >>= \x -> return ((return y) x) =
-- return f für x einsetzen
return ((return f) (return y)) = return (f y)


--Interchange: u <*> pure y = pure (\f -> f y) <*> u
--linke Seite zuerst
u <*> pure y =
    u >>= \f -> (pure y) >>= \x -> return (f x) = 
        -- pure = return
        u >>= \f -> (return y) >>= \x -> return (f x) =
            -- y mit \x füllen und f anwenden
            -- (return y) >>= \x -> return (f x)
            u >>= \f -> return (f y)

-- rechte Seite
pure (\f -> f y) <*> u =
    pure (\f -> f y) >>= \g -> u >>= \x -> return (g x) =
        -- return einsetzen
        return (\f -> f y) >>= \g -> u >>= \x -> return (g x)
        -- \f... für g einsetzen
        u >>= \x -> return ((\f -> f y) x)
        u >>= \x -> return (x y)

-- Da x und f beide Funktionen sind
 u >>= \f -> return (f y) = u >>= \f -> return (f y)



 -- Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
 --linke Seite
 pure (.) <*> u <*> v <*> w 
 (pure (.)) >>= \f -> u >>= \g -> v >>= \h -> w >>= \x -> return ((f g) (h x)) =
    --return für pure einsetzen
    (return (.)) >>= \f -> u >>= \g -> v >>= \h -> w >>= \x -> return ((f g) (h x)) =
        -- (.) für f einsetzen
        u >>= \g -> v >>= \h -> w >>= \x -> return (((.) g) (h x)) =
            -- solve function composition
            u >>= \g -> v >>= \h -> w >>= \x -> return (g (h x))
--rechte Seite
u <*> (v <*> w)
u >>= \g -> (v >>= \h -> w >>= \x -> return (h x)) >>= \y -> return (g y) =
    -- return (h x)) >>= \y -> return (g y) = return (g (h x))
    u >>= \g -> v >>= \h -> w >>= \x -> return (g (h x))


u >>= \g -> v >>= \h -> w >>= \x -> return (g (h x)) = u >>= \g -> v >>= \h -> w >>= \x -> return (g (h x))

