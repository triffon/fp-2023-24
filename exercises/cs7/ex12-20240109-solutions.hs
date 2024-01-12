-- примери за pattern matching за списъци
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort lst@(x:xs) = quickSort [ y | y<-xs, y < x]
                    ++           [ y | y<-lst, y == x]
                    ++ quickSort [ y | y<-xs, y > x]

quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy _ [x] = [x]
quickSortBy cmp lst@(x:xs) = quickSortBy cmp [ y | y<-xs, y `cmp` x]
                          ++                 [ y | y<-lst, not (x `cmp` y || y `cmp` x)]
                          ++ quickSortBy cmp [ y | y<-xs, x `cmp` y]
-- пример за сравняваща функция, която ще изчислява едно и също на всяко извикване
--sortByLen lsts = quickSortBy (\lst1 lst2 -> length lst1 < length lst2) lsts
-- решение: т.нар. two-step sort
-- изрично кешираме стойностите, за да не преизчислявамe, а само да достъпваме
quickSortOn :: Ord b => (a -> b) ->  [a] -> [a]
quickSortOn f lst = map fst $ quickSortBy (\p1 p2 -> snd p1 < snd p2) [ (x, f x) | x<-lst ]

-- за вкъщи -> вижте sort, sortBy, sortOn от библиотеката Data.List

-- Зад.1
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

-- проблем: разглеждайки всяка със всяка, повтаряме двойки
maxDistance :: [(Double, Double)] -> Double
maxDistance pts = maximum [ dist p1 p2 | p1<-pts, p2<-pts ]

-- решение: пробваме всяка точка само с тези след нея в списъка
maxDistance' :: [(Double, Double)] -> Double
maxDistance' [] = 0
maxDistance' (x:xs) = max (helper x xs) (maxDistance' xs)
  where helper p pts = maximum [ dist p p2 | p2<-pts ]

-- Зад.2
-- може тук да използваме ръчна рекурсия, ще е значително по-лесна
countMyHead :: Eq a => [a] -> Int
countMyHead lst = length $ takeWhile (\x -> x == head lst) lst

compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress lst = (head lst, n) : compress (drop n lst)
    where n = countMyHead lst

-- решение с рекурсия, поддържайки "състояние" от текущата последователност
-- наглед просто, а всъщност с много повече случаи за разглеждане :(
compress' :: Eq a => [a] -> [(a, Int)]
compress' [] = []
compress' lst = helper (head lst) 1 (tail lst)
  where helper curr count [] = [(curr,count)]
        helper curr count (x:xs)
          | curr == x   = helper curr (count+1) xs
          | otherwise   = (curr,count) : helper x 1 xs

-- Зад.3
maxRepeated :: Eq a => [a] -> Int
maxRepeated lst = maximum $ map snd $ compress lst

-- Зад.4
makeSet :: Eq a => [a] -> [a]
makeSet = foldl (\res el -> if el `elem` res then res else el:res) []

histogram :: Eq a => [a] -> [(a, Int)]
histogram lst = [ (x, count x) | x<-makeSet lst ]
  where count x = length [ y | y<-lst, y == x ]

-- Зад.5
specialSort :: Ord a => [[a]] -> [[a]]
specialSort lsts = quickSortOn mostFrequent lsts
  where mostFrequent lst = fst $ head $ quickSortBy (\p1 p2 -> snd p1 < snd p2) $ histogram lst
  -- заб: горното е неефективно, трябва ни аналогично maximumBy -> добре, че го има в Data.List

-- Алгебрични типове данни 101

-- Алтернативи, аналогични на enum-ите в C++
data Color = Red | Green | Blue

f :: Color -> Int
f Red = 5
f Green = 42
f Blue = 101

--data Bool = False | True

-- Алтернативи, всяка от която има някакви данни (мембъри)
-- => не можем без да искаме да достъпим грешните такива
data Shape = Circle (Double,Double) Double
           | Rect (Double,Double) (Double,Double)
           | Square (Double,Double) Double -- същите типове мембъри, но различна семантика

-- Най-мощният инструмент: pattern matching
area :: Shape -> Double
area (Circle (x,y) rad) = 3.14 * rad * rad
area (Rect (x,y) (w,h)) = w * h
area (Square (x,y) side) = side * side

-- само Shape е тип, Circle и Rect не са: те са само начини да създаваме обекти от тип Shape
s1, s2 :: Shape
s1 = Circle (4,5) 1
s2 = Rect (0,0) (3,2)

-- пример за конструиране на форми от други
translate :: Shape -> (Double, Double) -> Shape
translate (Circle (x,y) rad) (dx,dy) = Circle (x+dx,y+dy) rad
translate (Rect (x,y) (w,h)) (dx,dy) = Rect (x+dx,y+dy) (w,h)
translate (Square (x,y) side) (dx,dy) = Square (x+dx,y+dy) side
