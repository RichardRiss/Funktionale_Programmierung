import Control.Monad

-- Expression data type
data Exp
  = Lit Int
  | Var String
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp

-- Expression environment type
type ExpEnv = [(String, Int)]

-- Reader monad definition
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  Reader f <*> Reader a = Reader $ \r -> (f r) (a r)

instance Monad (Reader r) where
  return = pure
  (Reader h) >>= f = Reader $ \r -> runReader (f (h r)) r

-- MonadReader class definition
class Monad m => MonadReader r m  where
    ask   :: m r
    local :: (r -> r) -> m a -> m a

-- Instance of MonadReader for Reader
instance MonadReader r (Reader r) where
  ask = Reader id
  local f m = Reader $ \r -> runReader m (f r)

-- Empty environment function
emptyEnv :: ExpEnv
emptyEnv = []

-- Evaluation function
evalReader :: Exp -> Reader ExpEnv (Maybe Int)
evalReader (Lit i) = return (Just i)
evalReader (Var s) = do
  env <- ask
  case lookup s env of
    Just v -> return (Just v)
    Nothing -> return Nothing
evalReader (Add e1 e2) = evalBinaryOp (+) e1 e2
evalReader (Sub e1 e2) = evalBinaryOp (-) e1 e2
evalReader (Mul e1 e2) = evalBinaryOp (*) e1 e2
evalReader (Div e1 e2) = do
  me1 <- evalReader e1
  me2 <- evalReader e2
  case (me1, me2) of
    (Just n1, Just n2) -> if n2 == 0 then return Nothing else return (Just (n1 `div` n2))
    _ -> return Nothing

evalBinaryOp :: (Int -> Int -> Int) -> Exp -> Exp -> Reader ExpEnv (Maybe Int)
evalBinaryOp op e1 e2 = do
  me1 <- evalReader e1
  me2 <- evalReader e2
  return (op <$> me1 <*> me2)

eval :: Exp -> Maybe Int
eval exp = runReader (evalReader exp) emptyEnv


test :: IO()
test = do
    print(eval (Mul (Add (Lit 2) (Lit 3)) (Sub (Lit 10) (Lit 2))))