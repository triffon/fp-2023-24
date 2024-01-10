module Lazy where

import Prelude hiding (foldl')

ones :: [Integer]
ones = 1 : ones

-- >>> take 10 ones
-- [1,1,1,1,1,1,1,1,1,1]

-- !!! >>> length ones

-- >>> take 10 ([1..] ++ [1..])
-- [1,2,3,4,5,6,7,8,9,10]

-- >>> take 10 ([2, 4] ++ [1,3..])
-- [2,4,1,3,5,7,9,11,13,15]

-- >>> filter even ([2, 4] ++ [1,3..]) !! 0
-- 2

-- >>> filter even ([2, 4] ++ [1,3..]) !! 1
-- 4

-- >>> filter even ([2, 4] ++ [1,3..]) !! 2

-- >>> take 3 (filter even ([2, 4] ++ [1,3..]))

-- >>> head (filter even ([1,3..] ++ [2,4]))

--pairs :: [(Integer, Integer)]
-- pairs = [ (x, y) | x <- [0..], y <- [0..] ]

-- >>> take 10 pairs
-- [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9)]

-- pairs = [ (y, x - y) | x <- [0..], y <- [0..x]]
-- >>> take 10 pairs
-- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]

pairs :: [(Integer, Integer)]
pairs = concat [ if x == y then [(x,x)] else [(x, y), (y, x)] | x <- [0..], y <- [0..x]]

-- >>> take 10 pairs
-- [(0,0),(1,0),(0,1),(1,1),(2,0),(0,2),(2,1),(1,2),(2,2),(3,0)]

-- >>> :t ($)
-- ($) :: (a -> b) -> a -> b

-- f x = f (1 - x)
-- f x = seq x $ f (1 - x)
f x = f $! (1 - x)

-- >>> f 0

second :: p1 -> p2 -> p2
second _ y = y

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ nv []      = nv
foldl' op nv (x:xs) = (foldl' op $! nv `op` x) xs

x :: Integer
x = foldl' (+) 0 $ replicate 10000000 1
-- >>> x
-- 10000000
