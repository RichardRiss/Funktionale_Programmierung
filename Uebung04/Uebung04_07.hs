module Uebung04_07 where
    import System.IO
    import Data.Char (toLower)

    {-
    #######################
    hangman
    #######################
    -}

    -- Benötigte Datenstrukturen:
        -- Liste mit Secret
        -- Liste mit Guesses
        -- Integer mit Anzahl der Rateversuche -> Kann auch als Liste der Guesses genommen werden
    
    
    display :: String -> String -> String
    display secret guesses = [if toLower char `elem` guesses then char else '*' | char <- secret]


    hangman :: String -> IO ()
    hangman secret = mainHangman (map toLower secret) []

    mainHangman :: String -> String -> IO ()
    mainHangman secret guesses = do
        putStrLn $ "Secret: " ++ display secret guesses
        putStr "Enter a character: "
        x <- getChar
        putStrLn ""
        let guess = toLower x
        let newGuesses = guess : guesses
        if secret == display secret newGuesses
            then putStrLn $ "Solved in " ++ show (length newGuesses) ++ " tries."
            else mainHangman secret newGuesses


