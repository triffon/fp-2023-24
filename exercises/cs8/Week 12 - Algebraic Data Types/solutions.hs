type Name = String
type Age = Int
type Weight = Double

type Person = (Name, Age, Weight)

data Union = 
    First Int | 
    Second Double | 
    None | 
    Tuple (Union, Union) |
    Complex { name::String, weight::Double }

newtype FirstName = FirstName String

class (Eq t, Show t) => Number t where
    plus::t -> t -> t
    mult::t -> t -> t
    fromInt::Int -> t

data Nat = Zero | Succ Nat
    deriving (Eq, Ord, Show)

a::Nat
a = Succ $ Succ $ Succ $ Succ $ Succ Zero

instance Number Nat where
    plus::Nat -> Nat -> Nat
    plus Zero a = a
    plus (Succ a) b = Succ $ plus a b
    mult::Nat -> Nat -> Nat
    mult Zero _ = Zero
    mult (Succ Zero) a = a
    mult (Succ a) b = plus b $ mult a b
    fromInt::Int -> Nat
    fromInt n
        | n == 0 = Zero
        | otherwise = Succ $ fromInt (abs n - 1)

h::Int -> Nat
h = fromInt

-- instance Eq Nat where
--     (==):: Nat -> Nat -> Bool
--     Zero == Zero = True
--     Zero == Succ _ = False
--     Succ _ == Zero = False
--     Succ a == Succ b = a == b

cmp::Eq t => t -> t -> Bool
cmp a b = a == b

-- Eq Ord Read Show Enum

data BinaryTree t = Empty | Node t (BinaryTree t) (BinaryTree t)
    deriving Show

testTree::BinaryTree Integer
testTree = Node 5 
                (Node 1 
                      (Node 4 
                            Empty 
                            (Node 13 Empty Empty)) 
                      (Node 3 Empty Empty)) 
                (Node 8 
                      (Node 0 
                            (Node 10 Empty Empty) 
                            (Node 9 Empty Empty)) 
                      (Node 11 Empty Empty))

data BST t = BSTEmpty | BSTNode t (BST t) (BST t)

-- type Graph t = [(t, [t])]
newtype Graph t = Graph [(t, [t])] deriving Show

-- data Maybe t = Nothing | Just t

find::(t -> Bool) -> [t] -> Maybe t
find _ [] = Nothing
find pred (x:xs)
    | pred x = Just x
    | otherwise = find pred xs

fromInt'::Int -> Maybe Nat
fromInt' n
    | n == 0 = Just Zero
    | n > 0 = Just $ fromInt n
    | otherwise = Nothing

toInt::Nat -> Int
toInt Zero = 0
toInt (Succ a) = 1 + toInt a

slice::String -> Nat -> Nat -> Maybe String
slice str start end
    | length str <= toInt start ||
      length str < toInt (plus start end) = Nothing
    | otherwise = Just $ take (toInt end) $ drop (toInt start) str