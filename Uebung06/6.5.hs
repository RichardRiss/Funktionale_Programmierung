import Control.Monad


data Exp = Num Int
        | Exp :+: Exp
        | Exp :-: Exp
        | Exp :/: Exp
        | Exp :*: Exp
        | Var String

type ExpEnv = [(String, Int)]


newtype Reader r a = Reader (r -> a)

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  Reader f <*> Reader a = Reader $ \r -> (f r) (a r)

instance Monad (Reader r) where
  return = pure
  (Reader h) >>= f = Reader $ \r -> runReader (f (h r)) r

class Monad m => MonadReader r m  where
    ask   :: m r
    local :: (r -> r) -> m a -> m a

instance MonadReader r (Reader r) where
  ask = Reader id
  local f m = Reader $ \r -> runReader m (f r)

runReader :: Reader r a -> r -> a
runReader (Reader f) = f


-- Evaluation function
evalReader :: Exp -> Reader ExpEnv (Maybe Int)
evalReader (Num i) = return (Just i)
evalReader (Var s) = do
  env <- ask
  case lookup s env of
    Just v -> return (Just v)
    Nothing -> return Nothing
evalReader (e1 :+: e2) = do
  me1 <- evalReader e1
  me2 <- evalReader e2
  return ((+) <$> me1 <*> me2)
evalReader (e1 :-: e2) = do
  me1 <- evalReader e1
  me2 <- evalReader e2
  return ((-) <$> me1 <*> me2)
evalReader (e1 :*: e2) = do
  me1 <- evalReader e1
  me2 <- evalReader e2
  return ((*) <$> me1 <*> me2)
evalReader (e1 :/: e2) = do
  me1 <- evalReader e1
  me2 <- evalReader e2
  case (me1, me2) of
    (Just n1, Just n2) -> if n2 == 0 then return Nothing else return (Just (n1 `div` n2))
    _ -> return Nothing


eval :: Exp -> Maybe Int
eval exp = runReader (evalReader exp) []


test :: IO()
test = do
    print( eval ((Num 2 :+: Num 3) :*: (Num 10 :-: Num 2)) )