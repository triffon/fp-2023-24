module Main where

main::IO ()
main = getLine >>= print . unwords . reverse . words