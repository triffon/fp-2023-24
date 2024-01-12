{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Main where

parseList::IO [Int]
parseList = getLine >>= return . map read . words
-- parseList = do
--     line <- getLine
--     let numbers = words line
--     return $ map read numbers

sort::Ord t => [t] -> [t]
sort [] = []
sort (x:xs) = sort (filter (< x) xs) ++ [x] ++ sort (filter (>= x) xs)

main::IO ()
main = parseList >>= print . unwords . map show . sort
-- main = do
--     list <- parseList
--     let sorted = sort list
--     let numbers = map show sorted
--     mapM_ print numbers