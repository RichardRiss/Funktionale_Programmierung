{-
Assoziativgesetz für Listenmonade
-}


{-
    Proof of Monad laws
    1.
        return x >>= f = 
            [x] >>= f =
                 f x ++ ([] >>= f) =
                    f x ++ [] = 
                        f x 

    2. 
        xs >>= return =
           2 cases:
           a.) 
            xs = []
            [] >>= return =
                [] -> entspricht xs -> [] = xs
            b.)
             xs = y:ys
             (y:ys) >>= return 
             return y ++ (ys >>= return) =
                [y] ++ ys  wegen struktureller Induktion mit Annahme für Teilliste
                = y : ys entspricht xs
    
    3. 
        (xs >>= f) >>= g
        2 Cases in struktureller Induktion
        a.) xs = []
            ([] >>= f) >>= g =
                [] >>= g = 
                    [] =
                        [] >>= \x -> (f >>= g) 

        b.) xs = y:ys
            left side:
            (y:ys >>= f) >>= g =
                (f y ++ (ys >>= f)) >>= g = 
                    (f y >>= g) ++ ((ys >>= f) >>= g)
            
            right side:
            a >>= (\x -> f x >>= g)
            (y:ys) >>= (\x -> f x >>= g) =
                (f y ++ (ys >>= (\x -> f x >>= g))) =
                    (f y >>= g) ++ (ys >>= (\x -> f x >>= g))

            Um zu beweisen das recht und linke Seite gleich sind, strukturelle Induktion anwenden.
            Man nehme zuerst den hinteren Teil.
            Im einfachsten Fall (end case) ist nur noch eine leere Liste vorhanden und ergibt folgendes:
            linke Seite:
            ((ys >>= f) >>= g)
            ([] >>= f) >>= g = [] >>= g = []

            rechte Seite:
            (ys >>= (\x -> f x >>= g))
            ([] >>= (\x -> f x >>= g)) = []

            Im Induktionsschritt n + 1 passiert das gleiche wie oben:
            linke Seite:
            (y:ys >>= f) >>= g = (f y ++ (ys >>= f)) >>= g 

            rechte Seite:
            (y:ys) >>= (\x -> f x >>= g) = (f y ++ (ys >>= (\x -> f x >>= g))

            Es bildet sich die gleiche Struktur (quasi immer f y ++ eine Funktion)
            und für ein gleiches Verhalten für alle ys kann angenommen
            werden, dass (ys >>= f) = ys >>= (\x -> f x >>= g)

            Somit ist ((y:ys) >>= f) >>= g equivalent zu (y:ys) >>= (\x -> f x >>= g).

    -}



{-
Distributionsgesetz für MonadPlus-Instanz of list
-}

--         (Distributivgesetz)
--         (a `mplus` b) >>= f = a >>= f `mplus` b >>= f


-- xs >>= f = concat (map f xs)
-- mplus = (++)
-- concat (map f (a ++ b)) = concat (map f a) ++ concat (map f b)
-- map f (a ++ b) is equivalent to map f a ++ map f b


class Monad m => MonadPlus m where
    mzero :: m ()
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

{-
case a = mzero, b = [xs], f = map:
    left side:
    (a `mplus` b) >>= f
    ([] ++ [xs]) >>= map =
        map [xs]


    right side:
    a >>= f `mplus` b >>= f
    map [] ++ map [xs] =
        map [xs]

case a = [xs], b = mzero, f = map
    left side;
    (a `mplus` b) >>= f
    ([xs] ++ []) >>= map =
        map [xs]

    right side:
    a >>= f `mplus` b >>= f
    [xs] >>= map ++ [] >>= map =
        map [xs]

-}



{-
Gebt ein Übersetzung-Schema an, mit dem List-Comprehensions in die do-Notation übersetzt werden können
-}


-- [ expression | pattern <- list, predicate ] = 
--  do
--      pattern <- list
--      guard predicate
--      return expression
