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
