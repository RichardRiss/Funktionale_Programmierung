module Uebung04_08 where
    


    {-
    #######################
    do notation
    #######################
    -}


    getNonEmptyLine :: IO String
    getNonEmptyLine = 
        getLine >>= 
            (\input -> if null input 
                        then putStrLn "Please enter a non-empty string." >>
                            getNonEmptyLine
                        else return input)