--data Maybe a = Nothing | Just a

-- Зад.1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeUncons :: [a] -> Maybe (a,[a])
safeUncons [] = Nothing
safeUncons (x:xs) = Just (x,xs)

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix xs [] = Nothing
stripPrefix (x:xs) (y:ys)
  | x == y    = stripPrefix xs ys
  | otherwise = Nothing

findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex y (x:xs)
  | x == y    = Just 0
  -- | otherwise = add1ToMaybe $ findIndex y xs -- ако не се сетим за case
  -- | otherwise = succ <$> findIndex y xs
  | otherwise = case findIndex y xs of Nothing -> Nothing
                                       (Just idx) -> Just (idx + 1)
  where add1ToMaybe Nothing = Nothing
        add1ToMaybe (Just x) = Just (x + 1)

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = case f x of Nothing -> mapMaybe f xs
                                Just y -> y : mapMaybe f xs

test :: [String]
test = mapMaybe (stripPrefix "ab") ["abc", "abba", "xyz", "a", "ab"]

-- пример за функция, която приема Maybe като аргумент
f :: Maybe Int -> String
f Nothing = "нищо"
f (Just x) = "нещо: " ++ show x

data BinTree a = Empty | Node a (BinTree a) (BinTree a)

-- Зад.3
maxSumPath :: BinTree Int -> [Int]
maxSumPath Empty = []
maxSumPath (Node x l r)
  | sum maxLeft > sum maxRight   = x : maxLeft
  | otherwise                    = x : maxRight
  where maxLeft = maxSumPath l
        maxRight = maxSumPath r

-- Зад.4
prune :: BinTree a -> BinTree a
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node x l r) = Node x (prune l) (prune r)

-- Зад.5
bloom :: BinTree a -> BinTree a
bloom Empty = Empty
bloom (Node x Empty Empty) = Node x (Node x Empty Empty) (Node x Empty Empty)
bloom (Node x l r) = Node x (bloom l) (bloom r)

-- Зад.6
rotateLeft, rotateRight :: BinTree a -> BinTree a
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c)

-- Зад.7
treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap _ Empty = Empty
treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)
