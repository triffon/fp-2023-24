leftPad::[t] -> Int -> t -> [t]
leftPad lst n el = let diff = n - length lst
                   in replicate diff el ++ lst

join::[String] -> Char -> String
join (x:xs) delimeter = x ++ concatMap (delimeter:) xs

subsets::[t] -> [[t]]
subsets [] = [[]]
subsets (y:ys) = map (y:) (subsets ys) ++ subsets ys

insertionSort::Ord t => [t] -> [t]
insertionSort = foldr insert []
    where insert::Ord t => t -> [t] -> [t]
          insert el lst = [x | x <- lst, x < el] ++ [el] ++ [x | x <- lst, x >= el]

quickSortBy::Ord t => (t -> t -> Bool) -> [t] -> [t]
quickSortBy _ [] = []
quickSortBy cmp (x:xs) = let lhs = [y | y <- xs, cmp y x]
                             rhs = [y | y <- xs, not $ cmp y x]
                         in quickSortBy cmp lhs ++ [x] ++ quickSortBy cmp rhs

groupBy::Eq t2 => (t1 -> t2) -> [t1] -> [(t2, [t1])]
groupBy f lst = map (\x -> (x, filter ((==) x . f) lst)) $ nub $ map f lst
    where nub::Eq t => [t] -> [t]
          nub = foldr (\curr res -> if curr `elem` res then res else curr:res) []

permutations::Eq t => [t] -> [[t]]
permutations [] = [[]]
permutations lst = [x:xs | x <- lst, xs <- permutations $ removeElement x lst]
    where removeElement::Eq t => t -> [t] -> [t]
          removeElement el lst = [y | y <- lst, y /= el]

generateSumLists::Int -> Int -> [[Int]]
generateSumLists 1 s = [[s]]
generateSumLists k s = [x:xs | x <- [0..s], xs <- generateSumLists (k - 1) (s - x)]