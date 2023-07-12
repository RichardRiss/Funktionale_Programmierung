import Control.Monad

type Env a b = a -> Maybe b
type ExpEnv = Env String Int

type EnvState a = State ExpEnv a 

insert :: Eq a => a -> b -> Env a b -> Env a b
insert k v env = \k' -> if k == k' then Just v else env k'

data Exp = Num Int
         | Exp :+: Exp
         | Exp :/: Exp
         | Var String
         | Let String Int Exp

newtype State s a = ST (s -> (a,s))

instance Applicative (State s) where

  pure = return
  
  --(<*>) :: State s (a -> b) -> State s a -> State s b
  ST sf <*> ST sa = ST $ \s ->
     let (f,s1) = sf s
         (x,s2) = sa s1 in
       (f x, s2)
  
instance Functor (State s) where

  fmap f (ST sf) = ST $ \s ->
     let (x,s1) = sf s in
        (f x, s1)
  
instance Monad (State s) where

  -- return :: a -> State s a
  return x = ST (\s -> (x,s))

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  st_sf >>= f = ST $ \s -> 
     let ST sf = st_sf
         (x, s1) = sf s
         ST sg = f x in
       sg s1

get :: State s s
get = ST $ \s -> (s,s)

put :: s -> State s ()
put new_s = ST $ \_ -> ((),new_s) 

runState :: s -> State s a -> a
runState init (ST sf) =
   let (res, s_fin) = sf init in
     res 


{-
replace:
eval :: ExpEnv -> Exp -> Maybe Int
eval env (Num n)     = pure n
eval env (Var i)     = env i
eval env (e1 :+: e2) = (+) <$> eval env e1 <*> eval env e2
eval env (e1 :/: e2) = eval env e2 >>= \e' ->
  case e' of
    0 -> Nothing
    _ -> div <$> eval env e1 <*> pure e'
-}

evalState :: Exp -> EnvState (Maybe Int)
evalState (Num n) = return (Just n)
evalState (Var i) = get >>= \env -> return (env i)
evalState (e1 :+: e2) = do
  val1 <- evalState e1
  val2 <- evalState e2
  case (val1, val2) of
    (Just v1, Just v2) -> return (Just (v1 + v2))
    _ -> return Nothing
evalState (e1 :/: e2) = do
  x <- evalState e2
  case x of
    Just 0 -> return Nothing
    Just d -> do
      num <- evalState e1
      case num of
        Just n -> return (Just (n `div` d))
        Nothing -> return Nothing
    Nothing -> return Nothing
evalState (Let i v e) = do
  old_env <- get
  put (insert i v old_env)
  res <- evalState e
  put old_env
  return res


eval :: Exp -> Maybe Int
eval exp = let emptyEnv = \_ -> Nothing
           in runState emptyEnv (evalState exp)


-- Problem bei der Auswertung von Let "x" 42 (Var "x") :+: Var "x"
-- Var x existiert nur im Kontext der Let Expression, wird 
-- er in einem anderen Kontext verwendet existiert er nicht mehr


test :: IO()
test = do
    print(eval (Let "x" 5 (Var "x")))
    print(eval (Let "x" 39 ((Var "x") :+: (Num 30))))