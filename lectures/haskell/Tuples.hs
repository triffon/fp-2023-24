module Tuples where

type Point = (Double, Double)
type Vector = Point

p :: Point
p = (1.2, 3.4)

addVectors :: Vector -> Vector -> Vector
--addVectors v1 v2 = (fst v1 + fst v2, snd v1 + snd v2)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

v :: Vector
v = (5.6, 7.8)

-- >>> addVectors v v
-- (11.2,15.6)

-- >>> addVectors p v
-- (6.8,11.2)

-- >>> addVectors (1, 2) p
-- (2.2,5.4)

-- f :: Int -> ()
-- f _ = ()

g = g

-- >>> g
-- ProgressCancelledException

-- >>> fst (1, g)
-- 1

-- >>> (3)
-- 3
