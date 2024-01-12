module Main where

parseInt::IO Int
parseInt = read <$> getLine

main::IO ()
main = do
    -- num <- parseInt
    -- print (num + 10)
    line <- getLine
    let num = read line :: Int
    print (num + 10)