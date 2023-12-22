module Lists where

import Prelude hiding (head, tail, null, length,
                      (++), reverse, (!!), elem)

-- >>> :t [[]]
-- [[]] :: [[a]]
-- >>> :t tail [1]
-- tail [1] :: Num a => [a]

-- >>> :t tail
-- tail :: HasCallStack => [a] -> [a]

-- >>> :t (**2)
-- (**2) :: Floating a => a -> a

-- >>> :t ["Ivan", [2]]
-- No instance for `Num Char' arising from the literal `2'
-- In the expression: 2
-- In the expression: [2]
-- In the expression: ["Ivan", [2]]

-- >>> :t ""
-- "" :: String

-- >>> :t []
-- [] :: [a]

-- >>> [] == ""
-- True

-- >>> :t (==)
-- (==) :: Eq a => a -> a -> Bool

head :: [a] -> a
--head (x:_) = x
--head []    = error "Опит за вземане на глава на празен списък"

head l = case l of (x:_) -> x
                   []    -> error "Опит за вземане на глава на празен списък"

-- >>> head [[1,2], [3,4]]
-- [1,2]

-- >>> head []
-- Опит за вземане на глава на празен списък

tail :: [a] -> [a]
tail (_:xs) = xs
tail []     = error "Опит за вземане на опашка на празен списък"

-- >>> tail [[1,2], [3,4]]
-- [[3,4]]

-- >>> tail []
-- Опит за вземане на опашка на празен списък

null :: [a] -> Bool
null [] = True
null _  = False
-- null = (==[])

-- >>> null [1,2]
-- False

-- >>> null []
-- True

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

-- >>> length [1,2,3,4]
-- 4

-- >>> [1.2 .. 3.4]
-- [1.2,2.2,3.2]

-- >>> [1..0]
-- []

(++) :: [a] -> [a] -> [a]
l      ++ [] = l
[]     ++ l = l
(x:xs) ++ l = x:xs ++ l

-- >>> [1,2,3] ++ [4,5,6]
-- [1,2,3,4,5,6]

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

(!!) :: [a] -> Int -> a
[]     !! _ = error "Опит за вземане на елемент на некоректен индекс"
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n - 1)

-- >>> [1..10] !! 5
-- 6

-- >>> [1..10] !! (-5)
-- Опит за вземане на елемент на некоректен индекс

-- >>> [1..10] !! 15
-- Опит за вземане на елемент на некоректен индекс

elem :: Eq t => t -> [t] -> Bool
elem _ []     = False
elem n (x:xs) = n == x || elem n xs 

-- >>> elem 2 [1..10]
-- True

-- >>> elem 20 [1..10]
-- False

-- >>> :t 2
-- 2 :: Num a => a

pythagoreanTriples n = [(a,b,c) | b <- [1..n], a <- [1..b-1], c <- [1..n],
                                  a + b > c, a + c > b, b + c > a,
                                  a^2 + b^2 == c^2, gcd a b == 1 ]

-- >>> pythagoreanTriples 100
-- [(3,4,5),(5,12,13),(8,15,17),(20,21,29),(7,24,25),(12,35,37),(9,40,41),(28,45,53),(48,55,73),(33,56,65),(11,60,61),(16,63,65),(65,72,97),(36,77,85),(39,80,89),(13,84,85)]
