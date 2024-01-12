module Main where
import System.IO (readFile')

encode::String -> String
encode "" = ""
encode (x:xs)
    | x >= 'a' && x <= 'z' = toEnum ((fromEnum x - fromEnum 'a' + 13) `mod` 26 + fromEnum 'a') : encode xs
    | x >= 'A' && x <= 'Z' = toEnum ((fromEnum x - fromEnum 'A' + 13) `mod` 26 + fromEnum 'A') : encode xs
    | otherwise = x:encode xs

main::IO ()
main = do
    inputFile <- getLine
    outputFile <- getLine
    -- input <- readFile' inputFile
    input <- readFile inputFile
    writeFile outputFile $ encode input