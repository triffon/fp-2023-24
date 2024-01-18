import Data.Maybe (fromJust)
import Data.List (nub,elemIndex)
import Control.Monad.State (State,gets,modify,evalState)

-- от миналия път

-- Зад.8
-- Проблем - когато имаме произволен брой поддървета, няма смисъл някои от тях да са празни
-- => създаваме си помощен тип за непразно дърво с произволен брой наследници,
-- всеки от които е също непразно дърво. Листата просто ще имат празен списък от наследници
data NonEmptyNTree a = NNode a [NonEmptyNTree a]
-- Основният тип - дървото може да е празно, но поддърветата не.
-- Всъщност еквивалентен на Maybe, но е важно да ползваме newtype за да покажем че е разли§ен тип.
newtype NTree a = NTree (Maybe (NonEmptyNTree a))

ntreeSize :: NTree a -> Int
ntreeSize (NTree Nothing) = 0
ntreeSize (NTree (Just t)) = helper t
  where helper :: NonEmptyNTree a -> Int
        helper (NNode _ subtrees) = 1 + sum (map helper subtrees)

instance Functor NonEmptyNTree where
    fmap :: (a -> b) -> NonEmptyNTree a -> NonEmptyNTree b
    fmap f (NNode val subtrees) = NNode (f val) (fmap f <$> subtrees) -- fmap inside a fmap, yo
-- Maybe ни дава безплатна инстанция на Functor за цялото NTree

-- Зад.9
data BST a = BSTEmpty | BSTNode a (BST a) (BST a)

bstInsert :: Ord a => a -> BST a -> BST a
bstInsert val BSTEmpty = BSTNode val BSTEmpty BSTEmpty
bstInsert val (BSTNode x l r)
  | val < x    = BSTNode x (bstInsert val l) r
  | otherwise  = BSTNode x l (bstInsert val r)

bstSearch :: Ord a => a -> BST a -> Bool
bstSearch _ BSTEmpty = False
bstSearch val (BSTNode x l r)
  | val == x   = True
  | val < x    = bstSearch val l
  | otherwise  = bstSearch val r

bstValues :: BST a -> [a]
bstValues BSTEmpty = []
bstValues (BSTNode x l r) = bstValues l ++ [x] ++ bstValues r

bstSize :: BST a -> Int
bstSize BSTEmpty = 0
bstSize (BSTNode _ l r) = 1 + bstSize l + bstSize r

bstSort :: Ord a => [a] -> [a]
bstSort = bstValues . foldr bstInsert BSTEmpty

-- Зад.10
-- Причини да НЕ ползваме синоним на BST (k,v) за Map:
-- - не всяко BST е валиден асоциативен списък (не трябва да се повтарят ключове)
-- - BST (k,v) изисква Ord v, което е ненужно
-- - бихме извиквали погрешка функции за BST над Map-ове, които да го направят невалидно
-- => създаваме си отделен тип
data Map k v = MEmpty
             | MNode k v (Map k v) (Map k v)

mapInsert :: Ord k => k -> v -> Map k v -> Map k v
mapInsert key newVal MEmpty = MNode key newVal MEmpty MEmpty
mapInsert key newVal (MNode key' val left right)
  | key < key'  = MNode key' val (mapInsert key newVal left) right
  | key > key'  = MNode key' val left (mapInsert key newVal right)
  | otherwise   = MNode key' newVal left right

mapSearch :: Ord k => k -> Map k v -> Maybe v
mapSearch key MEmpty = Nothing
mapSearch key (MNode key' val left right)
  | key < key'  = mapSearch key left
  | key > key'  = mapSearch key right
  | otherwise   = Just val

-- За удобство
fromPairs :: Ord k => [(k,v)] -> Map k v
fromPairs = foldr (uncurry mapInsert) MEmpty

-- Зад.11
-- Да си личи, че след fmap-ване ключовете остават същите
instance Functor (Map k) where
  fmap :: (a -> b) -> Map k a -> Map k b
  fmap _ MEmpty = MEmpty
  fmap f (MNode key val left right) = MNode key (f val) (fmap f left) (fmap f right)

-- Зад.1
data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving Functor

values :: BinTree a -> [a]
values Empty = []
values (Node x l r) = values l ++ [x] ++ values r

labelTree :: Eq a => BinTree a -> BinTree Int
labelTree t = fmap f t
  where xs = nub $ values t
        f x = succ $ fromJust $ elemIndex x xs

-- Зад.2 с изрично пазене на състояние
-- и предаването му на ръка от всяка стъпка на следващата
-- (опасно и неудобно!)
labelTree' :: Eq a => BinTree a -> BinTree Int
labelTree' = fst . helper []
  where -- На всяко извикване ще връщаме и новото състояние
        helper s Empty = (Empty, s)
        helper s (Node x l r) = (Node idx newL newR, s3)
          where (newL, s1) = helper s l
                (idx, s2) = label x s1
                (newR, s3) = helper s2 r
        -- Трябва да се грижим за състоянието и в помощните функции, които извикваме
        label :: Eq a => a -> [a] -> (Int, [a])
        label x xs = (succ . fromJust . elemIndex x $ xs', xs')
          where xs' = if x `elem` xs then xs else xs++[x]

-- Монада State се грижи за това предаване, вкл. на
-- рекурсивните извиквания и други State функции.
-- Това е чиста функция, която вътре в себе си извиква
-- "нечистата", подавайки ѝ първоначално състояние.
labelTreeS :: Eq a => BinTree a -> BinTree Int
labelTreeS t = evalState (helper t) []
  where -- Функция, която връща BinTree Int, но използва състояние от тип [a]
        helper :: Eq a => BinTree a -> State [a] (BinTree Int)
        helper Empty = return Empty
        helper (Node x l r) = do
            newL <- helper l
            idx <- labelS x
            newR <- helper r
            return (Node idx newL newR)
        -- Тази функция връща просто Int, но също ползва (и променя) състоянието от тип [a]
        labelS :: Eq a => a -> State [a] Int
        labelS x = do
            modify $ \s -> if x `elem` s then s else s++[x]
            gets (succ . fromJust . elemIndex x)
