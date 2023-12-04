import Prelude hiding (length, map, filter, foldr)

length::[t] -> Int
length [] = 0
length (_:xs) = 1 + length xs

map::(t1 -> t2) -> [t1] -> [t2]
map _ [] = []
map f (x:xs) = f x:map f xs

filter::(t -> Bool) -> [t] -> [t]
filter _ [] = []
filter p (x:xs)
    | p x = x:filter p xs
    | otherwise = filter p xs

foldr::(t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr _ nv [] = nv
foldr op nv (x:xs) = op x (foldr op nv xs)

isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf l@(x:xs) (y:ys) = x == y && isInfixOf xs ys || isInfixOf l ys

maximumBy cmp = foldr1 (\curr res -> if res `cmp` curr then res else curr)

removeConsequtive [] = []
removeConsequtive (x:xs) = x:removeConsequtive (dropWhile (== x) xs)

pairSum n lst = [(x, y) | x <- lst, y <- lst, x <= y, x + y == n]

rationals = [(x - y, y) | x <- [0..], y <- [1..x]]

pythagoreanTriples = [(x, y, z) | z <- [1..], x <- [1..z], y <- [1..z], x^2 + y^2 == z^2, x < y]

generateExponents k l = [z | z <- [1..], x <- [1..z], y <- [1..z], z == x^k * y^l]