{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- no incomplete patterns in lambdas!


import Prelude hiding (foldl, zip, zipWith, takeWhile)
-- Типовете на елементите на списъка и акумулатора са различни
-- както ако в Scheme правите foldl върху списък от числа,
-- но с предикат и логическа връзка (and, or) натрупвате булева стойност
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl op acc (x:xs) = foldl op (op acc x) xs

-- За дадени два списъка [a1, ..., an] и [b1, ..., bn],
-- връща списъка от наредени двойки [(a1,b1), ..., (an,bn)]
-- Да се покрие и случаят, в който списъците имат различна дължина
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- Прилага поелементно функция върху двата списъка едновременно.
-- Връща списък от резултатите. Като map на 2 списъка в scheme
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

-- Връща най-големия префикс на списък,
-- такъв че даденият предикат е изпълнен за всичките му елементи
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs) = if p x then x : takeWhile p xs else []

-- за списък от цели числа, премахва дубликатите,
-- т.е. запазва само първите срещания на даден елемент
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/=x) xs)

-- Проверява дали число е просто
prime :: Int -> Bool
prime n
  | n < 2 = False
  | otherwise = null [x | x<-[2..n-1], n `rem` x == 0]

-- За дадено число n връща списък от първите n прости (положителни) числа
primes :: Int -> [Int]
primes n = take n $ filter prime [2..]

-- За дадено естествено число, връща списък от простите му делители
-- factorize 60 = [2, 2, 3, 5]
-- 2*2*3*5 = 60
factorize :: Int -> [Int]
factorize 1 = []
factorize n =
  let primeDivs = [x | x<-[2..n], prime x, n `rem` x == 0]
      first = head primeDivs
   in first : factorize (n `div` first)

-- quicksort за цели числа
-- Няма нужда да взимаме случаен елемент
-- просто можем да вземем първия
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =  lower ++ x : higher
  where lower = quicksort [y | y<-xs, y <= x]
        higher = quicksort [y | y<-xs, y > x]
