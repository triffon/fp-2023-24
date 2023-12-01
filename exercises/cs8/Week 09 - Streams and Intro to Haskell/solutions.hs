factorial::Int -> Int
factorial n = if n == 0 then 1 else n * factorial (n - 1)

-- pattern matching
factorial'::Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- guards
factorial''::Int -> Int
factorial'' n
    | n == 0 = 1
    | n > 0 = n * factorial'' (n - 1)
    | otherwise = error "bad argument"

roots::Double -> Double -> Double -> Int
roots a b c
    | discriminant a b c > 0 = 2
    | discriminant a b c == 0 = 1
    | otherwise = 0
    where discriminant::Double -> Double -> Double -> Double
          discriminant a b c = b ** 2 - 4 * a * c

repeat'::(t -> t) -> Int -> t -> t
repeat' _ 0 = id
repeat' f n = \ x -> f (repeat' f (n - 1) x)

modulus::(Double, Double) -> Double
modulus (a, b) = sqrt (a**2 + b**2)