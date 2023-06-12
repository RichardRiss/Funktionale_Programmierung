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
    {-

          
    diamond :: Int -> IO ()
    diamond 0 = return ()
    diamond 1 = putStrLn "*"
    diamond n = diamond' n (n*2 -1)
        where
            diamond' n m
                | m == 0 = do
                    return ()
                | m == (n*2 -1) || m == 1  = do
                    putStrLn((replicate ((n*2 - 1) `div` 2) ' ' ++ "*" ++ replicate ((n*2 - 1)`div` 2) ' '))
                    diamond' n (m-1)
                | otherwise = do      
                    putStrLn(replicate ((m `div` 2 -1)) ' ' ++ "*" ++ replicate ((m `div` 2 -1)) ' ' ++ "*" ++ replicate ((m `div` 2 -1)) ' ')
                    diamond' n (m-1)

                    -}
    diamond :: Int -> IO ()
    diamond n = mapM_ (putStrLn . line) [1-n..n-1]
        where
            line i = let absI = abs i in replicate (n - absI) ' ' ++ replicate (2 * absI + 1) '*'

