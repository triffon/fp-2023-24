module Main where

main::IO ()
main = do
    filename <- getLine
    word <- getLine
    contents <- readFile filename
    let allWords = words contents
    print $ length $ filter (== word) allWords
    -- readFile filename >>= print . length . filter (== word) . words