{-# LANGUAGE NamedFieldPuns #-}
-- turn warnings into errors
{-# OPTIONS_GHC -Werror #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

data MyBool
  = MyTrue
  | MyFalse
  deriving (Show)

data Colour
  = Red
  | Green
  | Cyan
  | Magenta
  | Yellow
  | Blue

data Shape
  = Circle Float
  | Rectangle Float Float
  | Ngon Int Float
  deriving (Show)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Show)

hello :: Person -> String
hello (Person {firstName, lastName}) =
  "Hello, " ++ firstName ++ " " ++ lastName

getArea :: Shape -> Float
getArea (Circle r) = pi * r * r
getArea (Rectangle a b) = a * b
getArea (Ngon _ _) = 42

isWarm :: Colour -> MyBool
isWarm Red = MyTrue
isWarm Yellow = MyTrue
isWarm Magenta = MyTrue
isWarm _ = MyFalse

instance (Show Colour) where
  show Red = "red"
  show Green = "green"
  show Cyan = "cyan"
  show Magenta = "magenta"
  show Yellow = "yellow"
  show Blue = "blue"

class MyEq a where
  (===) :: a -> a -> Bool

class Niceness a where
  howNice :: a -> Int

instance MyEq Int where
  (===) x y
    | x - y == 0 = True
    | otherwise = False

instance MyEq MyBool where
  (===) MyTrue MyTrue = True
  (===) MyFalse MyFalse = True
  (===) _ _ = False

instance Niceness Colour where
  howNice Red = 1
  howNice Yellow = 2
  howNice _ = 0

data List a
  = Cons a (List a)
  | Empty

myLen :: List a -> Int
myLen Empty = 0
myLen (Cons _ l) = 1 + myLen l

data Result a e
  = Ok a
  | Err e
  deriving (Show)

getAt :: Int -> List a -> Result a String
getAt 0 (Cons x _) = Ok x
getAt n (Cons _ t) = getAt (n - 1) t
getAt _ Empty = Err "no such element"

instance (Show a) => (Show (List a)) where
  show (Cons x xs) = "Cons " ++ (show x) ++ " " ++ (show xs)
  show Empty = "Empty"
