data MyList a = EmptyList | Construct a (MyList a)
    deriving (Show)

myCoolList :: MyList Int
myCoolList = Construct 1 (Construct 2 (Construct 3 EmptyList))

data Shape a = Circle a | Triangle a a a
    deriving (Show)

myCircle :: (Num a) => Shape a
myCircle = Circle 5

myTriangle :: (Fractional a) => Shape a
myTriangle = Triangle 5 2.2 4.33


data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show)