module Lists where

import Prelude hiding (head, tail, null, length,
                      (++), reverse, (!!), elem,
                      init, last, take, drop,
                      map, filter, foldr, foldl, foldr1, foldl1,
                      scanr, scanl, zip, unzip, zipWith)

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

-- >>> head [1..1032490328429048329048]
-- 1

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
{-
length []     = 0
length (_:xs) = 1 + length xs
-}

length = foldr (const (+1)) 0

-- >>> length [1,2,3,4]
-- 4

-- >>> [1.2 .. 3.4]
-- [1.2,2.2,3.2]

-- >>> [1..0]
-- []

(++) :: [a] -> [a] -> [a]
{-
l      ++ [] = l
[]     ++ l = l
(x:xs) ++ l = x:xs ++ l
-}

l1 ++ l2 = foldr (:) l2 l1

-- >>> [1,2,3] ++ [4,5,6]
-- [1,2,3,4,5,6]

reverse :: [a] -> [a]
{-
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
-}

-- reverse = foldr (\x r -> r ++ [x]) []
reverse = foldl (flip (:)) []
-- >>> reverse [1..5]
-- [5,4,3,2,1]

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
{-
elem _ []     = False
elem n (x:xs) = n == x || elem n xs
-}

elem n = foldr (\x -> (x == n ||)) False

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

last :: [a] -> a
{-
last []     = error "Не можем да върнем последния елемент на празен списък!"
last [x]    = x
last (_:xs) = last xs
-}

-- last = foldr (\_ r -> r) (error "...")
last = foldr1 (const id)
-- const = \x y -> x
-- >>> last [1..5]
-- 5



init []  = error "Не можем да върнем началото на празен списък!"
init [_] = []
init (x:xs) = x:init xs

-- >>> init [1..6]
-- [1,2,3,4,5]

take :: Integer -> [a] -> [a]
take _ []     = []
take 0 _      = []
take n (x:xs) = x:take (n-1) xs

-- >>> take 3 [1..10]
-- [1,2,3]

drop :: Integer -> [a] -> [a]
drop _ []     = []
drop 0 l      = l
drop n (_:xs) = drop (n-1) xs

-- >>> drop 3 [1..10]
-- [4,5,6,7,8,9,10]

-- >>> :t min
-- min :: Ord a => a -> a -> a

-- >>> :t minimum
-- minimum :: Ord a => [a] -> a

map :: (t -> a) -> [t] -> [a]
{-
map _ []     = []
map f (x:xs) = f x : map f xs
-}
map f = foldr (\x -> (f x:)) []

-- >>> map (*2) [1..5]
-- [2,4,6,8,10]

filter :: (a -> Bool) -> [a] -> [a]
{-
filter _ []     = []
filter p (x:xs)
 | p x       = x : fxs
 | otherwise = fxs
  where fxs = filter p xs
-}

filter p = foldr (\x -> if p x then (x:) else id) []

-- >>> filter even [1..10]
-- [2,4,6,8,10]

-- >>> [ x + y | x <- [1..5], y <- [1..x] ]
-- [2,3,4,4,5,6,5,6,7,8,6,7,8,9,10]

-- >>> concatMap (\x -> map (\y -> x + y) [1..x]) [1..5]
-- [2,3,4,4,5,6,5,6,7,8,6,7,8,9,10]

foldr1 :: (t -> t -> t) -> [t] -> t
foldr1 _ [x]     = x
foldr1 op (x:xs) = x `op` foldr1 op xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 op (x:xs) = foldl op x xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _  nv []     = nv
foldr op nv (x:xs) = x `op` foldr op nv xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
{-
scanr _  nv []     = [nv]
scanr op nv (x:xs) = x `op` r : h
  where h@(r:_) = scanr op nv xs
-}
scanr op nv = foldr (\x h@(r:_) -> x `op` r : h) [nv]

-- >>> scanr (+) 0 [1..6]
-- [21,20,18,15,11,6,0]

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ nv []      = nv
foldl op nv (x:xs) = foldl op (nv `op` x) xs

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _  nv []     = [nv]
scanl op nv (x:xs) = nv : scanl op (nv `op` x) xs

-- >>> scanl (+) 0 [1..6]
-- [0,1,3,6,10,15,21]

-- >>> zip [1..5] [2..10]
-- [(1,2),(2,3),(3,4),(4,5),(5,6)]

zip :: [a] -> [b] -> [(a, b)]
{-
zip _ []           = []
zip [] _           = []
zip (x:xs) (y:ys)  = (x,y):zip xs ys
-}

unzip :: [(a,b)] -> ([a],[b])
-- unzip l = (map fst l, map snd l)
unzip = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([], [])

-- >>> unzip [(1,2),(2,3),(3,4),(4,5),(5,6)]
-- ([1,2,3,4,5],[2,3,4,5,6])


zipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith _ _ []           = []
zipWith _ [] _           = []
zipWith op (x:xs) (y:ys)  = (x `op` y):zipWith op xs ys

zip = zipWith (,)

-- >>> zip [1..5] [2..5]
-- [(1,2),(2,3),(3,4),(4,5)]

-- >>> (\5 -> 10) (2 + 3)
-- 10
