module Functors where

import Prelude hiding (Functor,fmap,(<$>),
                       Applicative,pure,(<*>),
                       Monad, return, (>>=), (=<<))

import Data (BinTree, mapBinTree, leaves, bt)

-- >>> :k Maybe
-- Maybe :: * -> *

-- >>> :k Either
-- Either :: * -> * -> *

-- >>> :t []
-- [] :: [a]

-- >>> :k []
-- [] :: * -> *

-- >>> :k ([] Int)
-- ([] Int) :: *

-- >>> :k [Int]
-- [Int] :: *

-- >>> :t (,)
-- (,) :: a -> b -> (a, b)

-- >>> :k (,)
-- (,) :: * -> * -> *

-- >>> :k (,) Int String
-- (,) Int String :: *

-- >>> :k (Int, String)
-- (Int, String) :: *

-- >>> :k (,,)
-- (,,) :: * -> * -> * -> *

-- >>> :k (,,,)
-- (,,,) :: * -> * -> * -> * -> *

-- >>> :t (,) "Hello"
-- (,) "Hello" :: b -> (String, b)

-- >>> :t ("Hello",)
-- ("Hello",) :: t -> (String, t)

-- >>> :k (,) Int
-- (,) Int :: * -> *

-- >>> :k (Int,)
-- parse error on input `)'

type Matrix t = [[t]]

m :: [[Int]]
m = [[1]]

f :: Matrix t -> t
f ((x:_):_) = x

-- >>> f m
-- 1

newtype Dictionary k v = Dictionary [(k, v)]

-- >>> :t Dictionary
-- Dictionary :: [(k, v)] -> Dictionary k v

-- >>> :k Dictionary
-- Dictionary :: * -> * -> *

type DictSet t d = d t ()

type IntSet = DictSet Int Dictionary

-- >>> :k DictSet
-- DictSet :: k2 -> (k2 -> * -> k1) -> k1

-- >>> :k IntSet
-- IntSet :: *

-- >>> :k Int -> Int
-- Int -> Int :: *

-- >>> :k (->) Int Int
-- (->) Int Int :: *

-- >>> :k (->)
-- (->) :: * -> * -> *

-- >>> :k (->) Int ((->) Int Int)
-- (->) Int ((->) Int Int) :: *

-- >>> :k Int -> Int -> Int
-- Int -> Int -> Int :: *

class Countable c where
    count :: c a -> Int

-- >>> :k Countable
-- Countable :: (k -> *) -> Constraint

instance Countable [] where
    count = length

-- >>> count [1..10]
-- 10

instance Countable Maybe where
    count Nothing = 0
    count (Just _) = 1

-- >>> count (Just 2)
-- 1

-- >>> count (Just "abc")
-- 1

-- >>> count ['a'..'e']
-- 5

class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$>):: (a -> b) -> f a -> f b
    fmap = (<$>)
    (<$>) = fmap

instance Functor [] where
    fmap = map

instance Functor BinTree where
    fmap = mapBinTree

-- >>> fmap (+1) [1..10]
-- [2,3,4,5,6,7,8,9,10,11]

-- >>> bt
-- Node {root = 3, left = Node {root = 1, left = Empty, right = Empty}, right = Node {root = 5, left = Empty, right = Empty}}

-- >>> fmap (+1) bt
-- Node {root = 4, left = Node {root = 2, left = Empty, right = Empty}, right = Node {root = 6, left = Empty, right = Empty}}

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- >>> fmap (+1) (Just 2)
-- Just 3

-- >>> (+1) <$> [5..10]
-- [6,7,8,9,10,11]

-- >>> (*2) <$> Just 2
-- Just 4

-- >>> (+) <$> Just 2 <$> Just 3
-- Couldn't match expected type: a1_a2759[tau:1] -> b_a275a[sk:1]
--             with actual type: Maybe (a0_a275e[tau:1] -> a0_a275e[tau:1])
-- Possible cause: `(<$>)' is applied to too many arguments
-- In the first argument of `(<$>)', namely `(+) <$> Just 2'
-- In the expression: (+) <$> Just 2 <$> Just 3
-- In an equation for `it_a273a': it_a273a = (+) <$> Just 2 <$> Just 3
-- Relevant bindings include
--   it_a273a :: Maybe b_a275a[sk:1]
--     (bound at /home/trifon/fmisync/Courses/2023_24/FP_2023_24/sandbox/haskell/Functors.hs:157:2)

-- >>> :t (+) <$> Just 2
-- (+) <$> Just 2 :: Num a => Maybe (a -> a)

class Functor f => Applicative f where
    pure   :: a -> f a
    (<*>)  :: f (a -> b) -> f a -> f b
--    fmap = (<*>) . pure
--  (<$>) = (<*>) . pure

instance Applicative Maybe where
    -- pure   :: a -> Maybe a
    pure = Just
    -- (<*>)  :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just x = Just (f x)

-- >>> :t (+) <$> Just 2 <*> Just 3
-- (+) <$> Just 2 <*> Just 3 :: Num b => Maybe b

-- >>> (+) <$> Just 2 <*> Just 3
-- Just 5

-- >>> Just (+) <*> Just 2 <*> Just 3
-- Just 5

instance Applicative [] where
    -- pure   :: a -> [a]
    pure x = [x]
    -- (<*>)  :: [a -> b] -> [a] -> [b]
    fs <*> xs = -- [ f x | f <- fs, x <- xs] 
                concatMap (\f -> map (\x -> f x) xs) fs

-- >>> (+) <$> [1..3] <*> [2..5]
-- [3,4,5,6,4,5,6,7,5,6,7,8]

-- >>> liftA2 (+) [1..3] [2..5]
-- [3,4,5,6,4,5,6,7,5,6,7,8]

class Applicative m => Monad m where
    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Just x >>= f = f x

getAt :: Integer -> [a] -> Maybe a
getAt _ []  = Nothing
getAt 0 (x:_) = Just x
getAt n (_:xs) = getAt (n-1) xs

-- >>> getAt 2 [1..3]
-- Just 3

-- >>> getAt 4 [1..3]
-- Nothing

g x l = do y <- getAt x l 
           return $ x + y

-- >>> g 3 [3..10]    
-- Just 9

-- >>> g 10 [3..10]    
-- Nothing

instance Monad [] where
    return x = [x]

    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    -- xs >>= f = concatMap f xs
    (>>=) = flip concatMap 

-- >>> [1..5] >>= (\x -> [1..x])
-- [1,1,2,1,2,3,1,2,3,4,1,2,3,4,5]

-- concat :: [[a]] -> [a]

-- >>> :k (->) r
-- (->) r :: * -> *


instance Functor ((->) r) where
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap = (.)

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

instance Applicative ((->) r) where
    -- pure :: a -> r -> a
    -- pure x _ = x
    pure = const

    -- (<*>)  :: (r -> a -> b) -> (r -> a) -> r -> b
    (f <*> g) x = f x $ g x
    -- (<*>) = ap

instance Monad ((->) r) where
    -- (>>=) :: (r -> a) -> (a -> r -> b) -> r -> b
    f >>= g = flip g <*> f

-- join :: (r -> r -> a) -> r -> a
-- join f x = f x x

type State a b = (a, b)
