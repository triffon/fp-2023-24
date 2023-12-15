main :: IO()
main = do
    print $ (take 10 $ palindromePairs 4) == [(11, 11), (12, 21), (21, 12), (13, 31), (22, 22), (31, 13), (14, 41), (23, 32), (32, 23), (41, 14)]

nats :: [Int]
nats = [1 ..]

isPalindromePair :: Int -> (Int, Int) -> Bool
isPalindromePair n (x, y) = combined == reverse combined && length combined >= n
    where combined = (show x) ++ (show y)

palindromePairs :: Int -> [(Int, Int)]
palindromePairs n = [ (x, y) | s <- nats, x <- [1 .. s - 1], y <- [s - x], isPalindromePair n (x, y)]