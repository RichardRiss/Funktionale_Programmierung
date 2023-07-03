import Control.Monad (MonadPlus (..))

{-
    
-}

import Control.Monad (MonadPlus (..), guard)

insert :: MonadPlus m => a -> [a] -> m [a]
insert x xs = msum $ [ return $ front ++ [x] ++ back | i <- [0..length xs], let (front, back) = splitAt i xs ]

permutations :: MonadPlus m => [a] -> m [a]
permutations [] = return []
permutations (x:xs) = do
    ys <- permutations xs
    insert x ys
