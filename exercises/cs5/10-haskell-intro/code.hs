-- turn warnings into errors
{-# OPTIONS_GHC -Werror #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

foo :: Int
foo = 43

foos :: [[Int]]
foos = [[1, 2, 3], [4, 5]]

bar :: [(Char, Int, Bool)]
bar = [('a', 42, True), ('b', 43, False)]

add1' :: Int -> Int
add1' = \x -> x + 1

add1 :: Int -> Int
add1 x = x + 1

add :: Int -> (Int -> Int)
add x y = x + y

fact' :: Integer -> Integer
fact' x = if x == 0 then 1 else x * (fact' (x - 1))

fact'' :: Integer -> Integer
fact'' x
  | x == 0 = 1
  | otherwise = x * (fact' (x - 1))

fact :: Integer -> Integer
fact 0 = 1
fact x = x * (fact (x - 1))

myHead :: [a] -> a
myHead [] = error "cannot get head of empty list"
myHead (x : _) = x

myTail :: [a] -> [a]
myTail [] = error "cannot get tail of empty list"
myTail (_ : xs) = xs

isIntPrefix :: [Int] -> [Int] -> Bool
isIntPrefix [] _ = True
isIntPrefix _ [] = False
isIntPrefix (x : xs) (y : ys) = (x == y) && (isIntPrefix xs ys)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys) = (x == y) && (isPrefix xs ys)

myConcat :: [a] -> [a] -> [a]
myConcat [] l2 = l2
myConcat (x : xs) l2 = x : (myConcat xs l2)

frepeat :: Int -> (a -> a) -> a -> a
frepeat 0 _ x = x
frepeat n f x = frepeat (n - 1) f (f x)

frepeated :: Int -> (a -> a) -> (a -> a)
frepeated = frepeat
