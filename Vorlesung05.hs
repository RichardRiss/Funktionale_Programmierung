module Vorlesung05 where


    {-
    Monoidengensetze
    
    Left identity:	
    return a >>= h ≡	 h a
    
    Right identity:	
    m >>= return ≡ m
    
    Associativity:	
    (m >>= g) >>= h ≡ m >>= (\x -> g x >>= h) 

    >>= ist bind
    
    (>>, return ()) ist Monoid
    (>>) :: IO a -> IO b -> IO b

    Left ident: return x >>= f = f x
    right ident: act >>= return = act
    associativity:  (act >>= f) >>= g >>= act >>= (\x -> f x >>= g)

    
    
    (>>=,return) ist Monade
    (>>=) :: IO a -> (a -> IO b) -> IO b
    return :: a -> IO a


    Monoid ist spezifische Monade (mit Typen)


    -}

    main  = do
        putStr "Wie alt bist du?"
        ageStr <- getLine
        let age = read ageStr
        if age > 40 then
            putStrLn "alt"
        else
            putStrLn "nicht so alt"


    -- Unnötig kompliziertes Beispiel
    
    data Exp = Num Int
        | Exp :+: Exp
        | Exp :/: Exp
        deriving Show

    e1 = Num 42 :+: Num 31
    e2 = e1 :/: (e1 :+: Num (-73))

    eval :: Exp -> Maybe Int
    eval (Num n) = Just n
    eval (e1 :+: e2) =
        case eval e1 of
            Just v1 -> case eval e2 of
                Just v2 -> Just (v1 + v2)
                Nothing -> Nothing
            Nothing -> Nothing
    eval (e1 :/: e2) =
        case eval e2 of
            Just 0 -> Nothing
            Just v2 -> case eval e1 of
                Just v1 -> Just (v1 `div` v2)
                Nothing -> Nothing
            Nothing -> Nothing

    evalM :: Exp -> Maybe Int
    evalM (Num n) = return n
    evalM (e1 :+: e2) = do
        v1 <- evalM e1
        v2 <- evalM e2
        return (v1 + v2)
    evalM (e1 :/:e2) = do
        v2 <- evalM e2
        if v2 == 0 then
            Nothing
        else do
            v1 <- evalM e1
            return (v1 `div` v2)



    -- Jetzt als Monade
    eval (Num n) = return n
        eval (e1 :+: e2) = do
            v1 <- eval e1
            v2 <- eval e2
            return (v1 + v2)
        eval (e1 :/: e2) = do
            case eval e2 of
                Just 0 -> Nothing
                Just v2 -> do
                    v1 <- eval e1
                    return (v1 `div` v2)

    {-
    Maybe mit Fehlerbehandlung

        Nothing ist Fehler
        Just ist Erfolg mit Rückgabe x
        Der Fehler schlägt durch
    -}
    -- >> entspricht do 1, do 2
    -- >>= entspricht do  

    class Monad m where
        return  :: a -> m a
        (>>) :: m a -> m b -> m b
        (>>=) :: m a -> (a -> m b) -> m b
        mx >> my = my >>= \_ -> my

    instance Monad Maybe where
        -- return :: a -> Maybe a
        return = Just

        -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
        Nothing >>= _ = Nothing
        Just x >>= f = f x

    -- Anwendungsbeispiel?
    genfmap :: Monad m => (a -> b) -> m a -> m b
    -- Functorgesetze
    -- fmap id = id
    -- fmap (f . g) = fmap f . fmap g = fmap g (fmap g)
    genfmap f mx = mx >>= (\x -> return (f x))
 

    {-
    Zwischen Functor und Monade existiert noch Applicative
    "applikativer Functor"
    (<a>) :: f (a->b) -> f a -> f b
    pure :: a -> f a
    -}

    -- Anwendungsbeispiel applikativer Functor
    {- 
    class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b


    instance Applicative Maybe where
        (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
        Just f <*> Just x = Just (f x)
        _ <*> _ = Nothing


    -}
    evalA (Num n) = return n
    evalA (e1 :+: e2) = do
        pure (+) <*> (evalA e1) <*> (evalA e2)
    evalA (e1 :/: e2) = -- geht nicht mit Applikative nur mit Monade
        -- pure (\v1 v2 -> if v2 == 0 then Nothing else v1 `div` v2 )
        -- <*> evalA e2 <*>  evalA e1
        pure (flip div) <*> (case eval e2 of
            Just 0 -> Nothing
            mv2 -> mv2)
            <*> (evalA e1)


    {-
    proof of laws:
    1. return x >>= f = f x
        Just x >>= f = f x
    
    2. mx >>= return = mx

    Fall 1
    mx = Nothing
    Nothing >>= return = Nothging = mx

    Fall 2:
    mx = Just x
    Just x >> = return = return x = Just x

    3. (mx >>= f) >>= g = mx >>= (\x -> f x >>= g)
    Fall 1:
    mx = Nothing
    (Nothing >>= f) >>= g = Nothing >>= g = Nothing = mx

    Fall 2:
    mx = Just x
    (Just x >>= f) >>= g = Just x -> (\x -> )

    -}


    genfmapA :: Applicative f => (a -> b) -> f a -> f b
    genmapA g fx = pure g <*> fx  

    {-
    class Functor f => Applicative f where
        pure :: a -> f a

        (<*>) :: f (a -> b) -> f a -> f b

        (*>) :: f a -> f b -> f b

        (<*) :: f a -> f b -> f b

    (<$>) 

    -}
    


    -- Funktionale Listen
    -- reverse list
    rev :: [a] -> [a]
    rev xs = revH xs []
        where revH :: [a] -> ([a] -> [a])
            revH [] -> id
            revH (x:xs) -> revH xs . ([x] ++)


    -- Functional Lists:
    -- [] = id
    -- (++) = (.)

    newtype Flist a = Flist ([a] -> [a])

    toFlist :: [a] -> Flist a
    toList xs = (xs ++)


    fromList :: Flist a -> [a]
    fromList (Flist fs) = fs []

    instance Semigroup (Flist a) where
        --<> :: Flist a -> Flist a -> Flist a
        (<>) = (.)

    instance Monoid (Flist a) where
        mepmty = Flist id

    
    revl :: [a] -> [a]
    revl xs = fromList(revl xs)
        where revl :: [a] -> Flist a
            revl [] -> mepmty
            revl (x:xs) -> revl xs `mappend` (toFlist [x])



{--
    <$> - Functor
    -- (<$>) :: Functor f => (a -> b) -> f a -> f b
    > a = (*2)
    > b = Just 4
    > a <$> b
    Just 8


    <*> - Applicative
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    > a = Just (*2)
    > b = Just 4
    > a <*> b
    Just 8

--}