module Data where

-- >>> :t maxBound
-- maxBound :: Bounded a => a

-- >>> maxBound :: Int
-- 9223372036854775807

-- >>> maxBound :: Char
-- '\1114111'

-- >>> maxBound :: Integer
-- No instance for `Bounded Integer' arising from a use of `maxBound'
-- In the expression: maxBound :: Integer
-- In an equation for `it_aBdo': it_aBdo = maxBound :: Integer

-- >>> maxBound :: Bool
-- True

type UnaryFunction a = a -> a
type Matrix a = [[a]]
type Dictionary k v = [(k,v)]

-- >>> :t UnaryFunction
-- Illegal term-level use of the type constructor or class `UnaryFunction'
--   defined at /home/trifon/fmisync/Courses/2023_24/FP_2023_24/sandbox/haskell/Data.hs:20:1
-- In the expression: UnaryFunction

-- >>> :k UnaryFunction
-- UnaryFunction :: * -> *
-- >>> :k Dictionary
-- Dictionary :: * -> * -> *

class Measurable a where
    size :: a -> Integer
    empty :: a -> Bool
    empty x = size x == 0

-- >>> :k Measurable
-- Measurable :: * -> Constraint

larger :: Measurable a => a -> a -> Bool
larger x y = size x > size y

instance Measurable Integer where
    size 0 = 0
    size n = 1 + size (n `div` 10)

-- >>> size 1237
-- 4

-- >>> larger 1234 5678
-- False

-- >>> larger 12313 123
-- True

-- >>> empty 123
-- False

instance (Measurable a, Measurable b) => Measurable (a, b) where
    size (x, y) = size x + size y

-- >>> size (123, (456, 789))
-- 9

instance Measurable a => Measurable [a] where
{-
    size []     = 0
    size (x:xs) = size x + size xs 
-}
--    size = foldr ((+) . size) 0 
    size = sum . map size 

-- >>> size [1, 23123, 112]
-- 9

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read, Enum)

today :: Weekday
today = Wed

-- >>> today
-- Wed

type Name = String
type Score = Int
-- data Player = Player Name Score
data Player = Player { name :: String, score :: Int }
  deriving (Eq, Ord, Show, Read)

-- >>> :t Player
-- Player :: Name -> Score -> Player

-- >>> :k Player
-- Player :: *

katniss :: Player
katniss = Player "Katniss Everdeen" 45

-- >>> name katniss
-- "Katniss Everdeen"

-- >>> score katniss
-- 45

getName :: Player -> Name
getName (Player name _) = name

getScore :: Player -> Score
getScore (Player _ score) = score

type OtherPlayer = (Name, Score)
-- >>> :k OtherPlayer
-- OtherPlayer :: *

otherKatniss :: OtherPlayer
otherKatniss = ("Katniss Everdeen", 45)

otherGetName :: OtherPlayer -> Name
otherGetName = fst

otherGetScore :: OtherPlayer -> Score
otherGetScore = snd

-- !!! data Student = Student { name :: String, year :: Int}

data Shape = Circle { radius :: Double } | Rectangle { width, height :: Double }
  deriving (Eq, Ord, Show, Read)

-- >>> :k Shape
-- Shape :: *

circle :: Shape
circle = Circle 2.3

rectangle :: Shape
rectangle = Rectangle 3.5 1.8

area :: Shape -> Double
area (Circle r)      = pi * r^2
area (Rectangle w h) = w * h

-- >>> circle
-- Circle {radius = 2.3}

-- >>> circle == Circle 2.3
-- True

-- >>> katniss
-- Player {name = "Katniss Everdeen", score = 45}

-- >>> otherKatniss
-- ("Katniss Everdeen",45)

-- >>> [Mon .. Fri]
-- [Mon,Tue,Wed,Thu,Fri]

-- >>> Thu < Sat
-- True

-- >>> :t Just 5
-- Just 5 :: Num a => Maybe a

-- >>> :k Maybe
-- Maybe :: * -> *

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show, Read)

five :: Nat
five = Succ $ Succ $ Succ $ Succ $ Succ $ Zero

-- >>> five
-- Succ (Succ (Succ (Succ (Succ Zero))))

fromNat :: Nat -> Integer
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n  

-- >>> fromNat five
-- 5

-- >>> :t iterate
-- iterate :: (a -> a) -> a -> [a]
toNat :: Int -> Nat
toNat = (iterate Succ Zero !!)

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ $ plus m n

-- >>> fromNat $ plus five five 
-- 10

data Bin = One | BitZero Bin | BitOne Bin
  deriving (Eq, Ord, Show, Read)

six :: Bin
six = BitZero $ BitOne $ One

-- >>> six
-- BitZero (BitOne One)

fromBin :: Bin -> Integer
fromBin One = 1
fromBin (BitZero b) = 2 * (fromBin b)
fromBin (BitOne b)  = 2 * (fromBin b) + 1

-- >>> fromBin six
-- 6

succBin :: Bin -> Bin
succBin One         = BitZero One 
succBin (BitZero b) = BitOne b
succBin (BitOne  b) = BitZero $ succBin b

--- >>> succBin six
-- BitOne (BitOne One)

--- >>> succBin $ succBin six
-- BitZero (BitZero (BitZero One))

plusBin :: Bin -> Bin -> Bin
plusBin One b                     = succBin b
plusBin b   One                   = succBin b
plusBin (BitZero b1) (BitZero b2) = BitZero (plusBin b1 b2)
plusBin (BitOne  b1) (BitZero b2) = BitOne (plusBin b1 b2)
plusBin (BitZero b1) (BitOne b2)  = BitOne (plusBin b1 b2)
plusBin (BitOne b1)  (BitOne b2)  = BitZero $ succBin $ plusBin b1 b2

-- >>> fromBin $ plusBin six six
-- 12

data List a = Nil | Cons { listHead :: a, listTail :: List a }
  deriving (Eq, Ord, Show, Read)

l :: List Integer
l = Cons 1 $ Cons 2 $ Cons 3 $ Nil

-- >>> listHead l
-- 1

-- >>> listTail l
-- Cons {listHead = 2, listTail = Cons {listHead = 3, listTail = Nil}}

fromList :: List a -> [a]
fromList Nil         = []
fromList (Cons x xs) = x : fromList xs

-- >>> fromList l
-- [1,2,3]

lengthList :: List a -> Nat
lengthList Nil         = Zero
lengthList (Cons _ xs) = Succ $ lengthList xs

-- >>> lengthList l
-- Succ (Succ (Succ Zero))

data BinTree a = Empty | Node { root :: a, left, right :: BinTree a }
  deriving (Eq, Ord, Show, Read)

leaf :: a -> BinTree a
leaf x = Node x Empty Empty

bt :: BinTree Integer
bt = Node 3 (leaf 1) (leaf 5)

-- >>> bt
-- Node {root = 3, left = Node {root = 1, left = Empty, right = Empty}, right = Node {root = 5, left = Empty, right = Empty}}

depth :: BinTree a -> Integer
depth Empty        = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

-- >>> depth bt
-- 2

leaves :: BinTree a -> [a]
leaves Empty                = []
leaves (Node x Empty Empty) = [x]
leaves (Node x l     r)     = leaves l ++ leaves r

-- >>> leaves bt
-- [1,5]

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _ Empty = Empty
mapBinTree f (Node x l r) = Node (f x) (mapBinTree f l) (mapBinTree f r)

-- >>> mapBinTree (+1) bt
-- Node {root = 4, left = Node {root = 2, left = Empty, right = Empty}, right = Node {root = 6, left = Empty, right = Empty}}

data Tree a     = Tree { rootTree :: a, subtrees :: TreeList a }
data TreeList a = None | SubTree { firstTree :: Tree a,
                                   restTrees :: TreeList a}

leafTree :: a -> Tree a
leafTree x = Tree x None

tree :: Tree Integer
tree = Tree 1 $ SubTree (leafTree 2)
              $ SubTree (Tree 3 $ SubTree (leafTree 4) $ None)
              $ SubTree (leafTree 5) $ None

level :: Integer -> Tree a -> [a]
level 0 (Tree x _ ) = [x]
level n (Tree _ ts) = levelTrees (n-1) ts

levelTrees :: Integer -> TreeList a -> [a]
levelTrees _ None = []
levelTrees n (SubTree t ts) = level n t ++ levelTrees n ts

-- >>> level 1 tree
-- [2,3,5]

data SExpr = SBool Bool | SChar Char | SInt Int |
             SDouble Double | SList { list :: [SExpr] }
             deriving (Eq, Ord, Show, Read)
sexpr = SList [SInt 2, SChar 'a', SList [SBool True, SDouble 1.2, SList []]]

countAtoms :: SExpr -> Integer
countAtoms (SList ses) = sum $ map countAtoms ses
countAtoms _         = 1

-- >>> countAtoms sexpr
-- 4

-- >>> :t list
-- list :: SExpr -> [SExpr]

flatten :: SExpr -> SExpr
flatten (SList ses) = SList $ concatMap (list . flatten) ses
flatten se        = SList [se]

-- >>> flatten sexpr
-- flatten sexpr :: SExpr
