module Uebung05_01 where
    import System.IO
    import Data.Char (toLower)
    import System.Directory (doesFileExist)
    
    {-
    #######################
    hangman
    #######################
    -}


    readWordlist :: FilePath -> IO [String]
    readWordlist path = do
        fileExists <- doesFileExist path
        if fileExists
            then do
                contents <- readFile path
                return $ lines contents
            else do
                putStrLn("File " ++ path ++ " does not exist.")
                return []

    display :: String -> String -> String
    display secret guesses = [if toLower char `elem` guesses then char else '*' | char <- secret]

    printList :: Int -> [String] -> IO ()
    printList _ [] = return ()
    printList i (x:xs) = do 
        putStrLn(x ++ " : " ++ show i) 
        printList (i+1) xs


    hangman :: IO ()
    hangman = do
        wordlist <- readWordlist "./woerterbuch.txt"
        putStrLn "Choose a word by entering a number: "
        --printList 1 wordlist
        putStrLn $ "Choose a value between 1 and " ++ show (length wordlist)
        choice <- getLine
        let secret = wordlist !! (read choice - 1)
        mainHangman (map toLower secret) []

    mainHangman :: String -> String -> IO ()
    mainHangman secret guesses = do
        putStrLn $ "Secret: " ++ display secret guesses
        putStr "Enter a character: "
        x <- getChar
        putStrLn ""
        let guess = toLower x
        let newGuesses = guess : guesses
        if secret == display secret newGuesses
            then do
                putStrLn $ "Secret " ++ secret ++ " solved correctly."
                putStrLn $ "Solved in " ++ show (length newGuesses) ++ " tries."
                appendFile "ergebnisse.csv" (secret ++ "," ++ show(length newGuesses))
            else mainHangman secret newGuesses