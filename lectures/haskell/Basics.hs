module Basics where

x :: Integer
x = 2

-- >>> x * 50
-- 100

y :: Double
y = 1.2

-- >>> fromInteger(x) + y
-- 3.2

-- >>> :t x
-- x :: Integer
-- >>> :t fromInteger(x)
-- fromInteger(x) :: Num a => a

-- >>> x^2 + 4.5
-- No instance for `Fractional Integer' arising from the literal `4.5'
-- In the second argument of `(+)', namely `4.5'
-- In the expression: x ^ 2 + 4.5
-- In an equation for `it_asRg': it_asRg = x ^ 2 + 4.5
-- >>> :t 2
-- 2 :: Num a => a

-- >>> x / 3
-- No instance for `Fractional Integer' arising from a use of `/'
-- In the expression: x / 3
-- In an equation for `it_at2Z': it_at2Z = x / 3

-- >>> :t (^)
-- (^) :: (Num a, Integral b) => a -> b -> a

-- >>> :t (^^)
-- (^^) :: (Fractional a, Integral b) => a -> b -> a

-- >>> :t (**)
-- (**) :: Floating a => a -> a -> a

-- >>> sin    (((2)))
-- 0.9092974268256817

twice :: (t -> t) -> t -> t
twice f x = f (f x)

diag :: (t1 -> t1 -> t2) -> t1 -> t2
diag f x = f x x

square x = x * x

-- >>> square (-x)
-- 4
