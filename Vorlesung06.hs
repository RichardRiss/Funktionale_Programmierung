module Vorlesung06 where
import Data.Sequence (Seq(Empty))


    {-
    3 Monadengesetze:
    (einstelliger Typkontruktur, bind, neutrales Element)
    (m,(>>=),return)
        return :: a -> m a
        >>= :: m a -> f (a -> m b) - m b

        1. return x >>= f = f x
        2. a >>= return = a
        3. (a >>= f) >>= g = a >>= \x -> f x >>= g


    Functor

    ^
    |

    Applicative ( pure,(<*>) )
        (<*>) :: f(a->b) -> f a -> f b
        pure :: a -> f a
        
    ^
    |
   
    Monad


    -}

    -- Listendatentyp als Applikative und Monad

    {-
    
    instance Applicative [] where
        pure :: a -> [a]
        pure x -> [x]

        (<*>) :: [a -> b] -> [a] -> [b]
        [] <*> xs = []
        (f:fs) <*> xs = fmap f xs ++ (fs <*> xs)

    instance Monad [] where 
        return a -> [a]
        return :: pure

        (>>=) :: [a] -> (a->[a]) -> [b]
        [] >>= f = []
        (x:xs) >>= f = f x ++ (xs >>= f)

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

            Es bildet sich die gleiche Struktur und für ein gleiches Verhalten für alle ys kann angenommen
            werden, dass (ys >>= f) = ys >>= (\x -> f x >>= g)

            Somit ist ((y:ys) >>= f) >>= g equivalent zu (y:ys) >>= (\x -> f x >>= g).

                    
    
    -}

    list = [1,2,3] >>= \x -> [x,x+1]
    -- als do
    list1 = do
        x <- [1,2,3]
        [x,x+1]
        
    list2 = [1,2,3,4] >>= \x -> [3,4,5] >>= \y -> return (x,y)
    -- als do
    list3 = do
        x <- [1,2,3,4]
        y <- [3,4,5]
        return (x,y)
    -- als list comprehension
    list2' = [(x,y)| x <-[1,2,3,4], y <- [3,4,5] ]



    -- Neu List comprehension mit Bedingung (Guard)
    list3' = [(x,y)| x <-[1,2,3,4], y <- [3,4,5], x /= y ]
    --entspricht
    list3'' = do 
        x <- [1,2,3,4]
        y <- [3,4,5]
        --if x /= y then
        --    return (x,y)
        --else 
        --   []
        guardList (x/=y)
        return (x,y)

    guardList :: Bool -> [()]
    guardList True = return () --[()]
    guardList False = []



    {-
    Listen sind Monaden mit return und bind (>>=)

    Listen sind auch Monoide mit mempty und mappend (++)

    beides zusammen ist in der Klasse MonadPlus zusammen geführt

    class Monad m => MonadPlus m where
        mzero :: m ()
        mappend :: m a -> m a -> m a

    instance MonadPlus [] where
        mzero = []
        mappend = (++)

    guard :: MonadPlus m => Bool -> m ()
    guard True = return ()
    guard False = mzero


    Implementierungen mit guard in Ghc.Base.Alternative:
        Alternative entspricht Implementierung von Applicative
        MonadPlus entspricht Implementierung von Monad

    -}

    {-
    Monad Instanz für Maybe:
    return = Just
    Nothing >>= _ = Nothing
    (Just y >>= f) = f x

    instance MonadPlus Maybe wgere
        mzero = Nothing
        Nothing `mplus` my = my
        Just x `mplus` my = Just x
    
    Gesetz für MonadPlus:
        (normale Monoidgesetze)
            mzero `mplus` a = a
            a `mplus`mzero = a
            (a `mplus` b) `mplus` c = a `mplus` (b `mplus` c)
        (Distributivgesetz)
            (a `mplus` b) >>= f = a >>= f `mplus` b >>= f
    
    Satz: Gesetze gelten für die Listeninstanz


    Aber für Maybe-Instanz gilt das Distributivgesetz nicht:
        Just False `mplus` Just True >>= guard =
            Just False >>= guard = 
                mzero = 
                    Nothing

        (Just False >>= guard) `mplus` (Just True >>= guard) =
            Nothing `mplus` Just () =
                Just ()


    Distributivegesetz existiert auch für Alternative. Maybe-Instanz erfüllt die Maybe-Instanz das Distr.-gesetz.
    Genau das macht den Unterschied zw. Alternative und Monade aus.
    Distributivegesetz für Alternative:
    (cf </> cg) <*> cx = cf <*> cx </> cg <*> cx
    cf <*> (ca </> cb) = cf <*> ca </> cf <*> cb

    
    -}



    -- bisschen Programm
    data Tree a = Node (Tree a) a (Tree a)
                    | Empty
                deriving Show

    testTree = Node (Node Empty 42 Empty) 73 (Node Empty 55 Empty)

    -- soll raus kommen:
    -- Node (Node Empty (0,42) Empty) (1,73) (Node Empty (2,55) Empty)
    numberTree :: Tree a -> Tree (Int,a)
    numberTree t = fst help t 0
        where help :: Tree a -> Int -> (Tree (Int,a),Int)
            help Empty label = (Empty, label)
            help (Node tl x tr) label =
                let (tl',label1) = help tl label
                    (tr',label2) = help tr (label + 1)
                in
                    (Node tl' (label1, x) tr', label2) 


    -- unschöner Code
    -- deshalb Case-Monade

    newtype State a = ST(Int -> (a,Int))

    numberTreeM st = runState 0 (help t)
        where help :: Tree a -> State (Tree (Int,a))
            help Empty = return Empty
            help (Node tl x tr) = do
                tl' <- help tl
                n <- get
                put (n + 1)
                tr' <- help tr
                return (Node tl' (n,x) tr')

    
    -- Monadeninstanz
    instance Monad State where
        -- return :: a -> State a 
        return x = ST \s -> (x,s)

        -- (>>=) :: State a -> (a -> State b) -> State b
        ST sf >>= f = ST (\s ->
            let (x,s1) = sf s
                ST sg = f x
                (res,s2) = sg s1 in
                    (res,s2))  

    -- Applicativeinstanz
    instance Applicative State where
        pure = return
        --(<*>) = ap
        (<*>) :: State s (a->b) -> Sate s a -> State s b
        ST sf <*> ST sa = ST $ \s -> 
            let (f,s1) = sf s
                (x,s2) = sa s1 
                in
                    (f x, s2)

    -- Funktorinstanz
    instance Functor State where
        fmap = (<$>)


    get :: Sate Int
    get = ST \s -> (s,s)

    put :: Int -> State ()
    put = new_s = ST \s -> ((),new_s)

    runState :: Int -> Sate a -> a
    runState init (ST sf) =
        let (res, s_fin) = sf init in
            res  


