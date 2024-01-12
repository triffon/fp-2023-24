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

data MyBool = MyTrue | MyFalse deriving (Show, Eq)

data RPSLS
  = Rock
  | Paper
  | Scissors
  | Lizard
  | Spock

myAnd :: MyBool -> MyBool -> MyBool
myAnd MyTrue MyTrue = MyTrue
myAnd _ _ = MyFalse

-- class Show a where
--   show :: a -> String

class Numerisable a where
  toNum :: a -> Int

instance Numerisable RPSLS where
  toNum Rock = 0
  toNum Paper = 1
  toNum Scissors = 2
  toNum Lizard = 5
  toNum Spock = 42

data Shape
  = Circle Float
  | Rectangle Float Float
  | Ngon Int Float
  deriving (Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Ngon n _)
  | n >= 100 = True
  | otherwise = False
isRound (Rectangle _ _) = False

data Result e a = Ok a | Err e deriving (Show)

-- class Functor c where
--   fmap :: (a -> b) -> c a -> c b

instance Functor (Result e) where
  fmap f (Ok x) = Ok $ f x
  fmap _ (Err err) = Err err

getBest :: (Numerisable a) => [a] -> Either String a
getBest [] = Left "empty list given to getBest"
getBest [x] = Right x
getBest (x : xs) = maybeReplaceByX <$> (getBest xs)
  where
    -- maybeReplaceByX :: a -> a
    maybeReplaceByX y
      | toNum x > toNum y = x
      | otherwise = y

instance Show RPSLS where
  -- show :: RPSLS -> String
  show Rock = "rock"
  show Paper = "paper"
  show Scissors = "scissors"
  show Spock = "spock"
  show Lizard = "lizard"

-- Source (text) ----- parser ----> AST ---> IR (bytecode) -- codegen --> machine code

data CalcAST
  = Num Int
  | UnaryOP UnaryOP CalcAST
  | BinaryOP BinaryOP CalcAST CalcAST
  deriving (Show)

data UnaryOP
  = Negate
  | Sqrt
  deriving (Show)

data BinaryOP
  = Plus
  | Minus
  | Mul
  | Div

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Show)

pesho :: Person
pesho =
  Person
    { firstName = "Petar",
      lastName = "Petkov",
      age = 26
    }

hello :: Person -> String
hello (Person {firstName = fn, lastName = ln}) =
  "Hello, " <> fn <> " " <> ln <> "!"

instance Show BinaryOP where
  show Plus = "+"
  show Minus = "-"
  show Mul = "*"
  show Div = "/"

myExpr :: CalcAST
myExpr = UnaryOP Sqrt $ BinaryOP Plus (Num 1) (Num 2)

prettyPrint :: CalcAST -> String
prettyPrint (Num x) = show x
prettyPrint (UnaryOP op expr) = (show op) <> "(" <> (prettyPrint expr) <> ")"
prettyPrint (BinaryOP op e1 e2) = "(" <> (prettyPrint e1) <> " " <> (show op) <> " " <> (prettyPrint e2) <> ")"

interpretCalc :: CalcAST -> Int
interpretCalc = undefined -- ....
