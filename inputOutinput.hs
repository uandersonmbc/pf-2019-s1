import Control.Monad  
import Data.Char

main = putStrLn "hello, world"

main1 = do
    text <- readFile "arquivo.txt"
    putStrLn text

main2 = do  
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main2
        else return ()

main3 = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]

main4 = forever (do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l)
    