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

answer :: Int
answer = 42

add1 :: Int -> Int
add1 x = x + 1

add :: Int -> (Int -> Int)
add x y = x + y

b :: Char
b = 'b'

foo :: [Int]
foo = [1, 2, 3, 4]

fact :: Integer -> Integer
fact 0 = 1
fact x = x * (fact (x - 1))

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

myAbs :: Int -> Int
myAbs x
  | x < 0 = 0 - x
  | otherwise = x

composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
composeInt f g x = f (g x)

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x)

myConcat :: [a] -> [a] -> [a]
myConcat [] l2 = l2
-- myConcat l1 l2 = (head l1) : (myConcat (tail l1) l2)
myConcat (x : xs) l2 = x : (myConcat xs l2)

isIntPrefix :: [Int] -> [Int] -> Bool
isIntPrefix [] _ = True
isIntPrefix _ [] = False
isIntPrefix (x : xs) (y : ys) = (x == y) && (isIntPrefix xs ys)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys) = (x == y) && (isPrefix xs ys)

frepeat :: Int -> (a -> a) -> a -> a
frepeat 0 _ x = x
frepeat n f x = frepeat (n - 1) f (f x)

frepeated :: Int -> (a -> a) -> (a -> a)
frepeated = frepeat
