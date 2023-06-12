module Uebung04_06 where
    


    {-
    #######################
    geometric figures
    #######################
    -}


    -- square
    -- square :: Int -> IO ()
    {-
    square 4 =
        ****
        *  *
        *  *
        ****
    -}
    square :: Int -> IO ()
    square n = square' n n
        where
            square' n 0 = return ()    
            square' n m
                | n == m || m == 1 = do
                    putStrLn (replicate n '*')
                    square' n (m-1)
                | n /= m = do 
                    putStrLn ("*" ++ replicate (n-2) ' ' ++ "*")
                    square' n (m-1)


    -- diamond
    -- diamond :: Int -> IO ()
          
    diamond :: Int -> IO ()
    diamond 0 = return ()
    diamond 1 = putStrLn "*"
    diamond n = diamond' n 1 False
        where
            diamond' n row dir
                | row == 0 && dir = do
                    return ()
                | row == 1 && not dir  = do
                    putStrLn(replicate (n - 1) ' ' ++ "*")
                    diamond' n (row + 1) dir
                | row == 1 && dir = do
                    putStrLn(replicate (n - 1) ' ' ++ "*")
                    diamond' n (row - 1) dir
                | row == n = do
                    putStrLn (replicate (n - row) ' ' ++ "*" ++ replicate (row * 2 - 3) ' ' ++ "*")
                    diamond' n (row - 1)  (not dir)
                | dir = do
                    putStrLn (replicate (n - row) ' ' ++ "*" ++ replicate (row * 2 - 3) ' ' ++ "*")
                    diamond' n (row - 1) dir
                | not dir = do
                    putStrLn (replicate (n - row) ' ' ++ "*" ++ replicate (row * 2 - 3) ' ' ++ "*")
                    diamond' n (row + 1) dir


                