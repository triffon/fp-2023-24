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

bst :: BST Integer
bst = BSTNode 3 (BSTNode 1
                         BSTEmpty
                         (BSTNode 2 BSTEmpty BSTEmpty))
                (BSTNode 4
                         BSTEmpty
                         (BSTNode 5 BSTEmpty BSTEmpty))

instance Functor BinaryTree where
    fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
    fmap _ Empty = Empty
    fmap f (Node root left right) = Node (f root) (fmap f left) (fmap f right)

instance Foldable BinaryTree where
    foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
    foldr _ nv Empty = nv
    foldr op nv (Node root left right) = foldr op (op root $ foldr op nv right) left

rotateLeft::BinaryTree t -> BinaryTree t
rotateLeft Empty = Empty
rotateLeft t@(Node _ _ Empty) = t
rotateLeft (Node a x (Node c y z)) = Node c (Node a x y) z

bloom::BinaryTree t -> BinaryTree t
bloom Empty = Empty
bloom (Node root Empty Empty) = Node root (Node root Empty Empty) (Node root Empty Empty)
bloom (Node root left right) = Node root (bloom left) (bloom right)

commonAncestor::Eq a => a -> a -> BinaryTree a -> Maybe a
commonAncestor u v tree = let pathU = pathTo u tree
                              pathV = pathTo v tree
                          in leastCommon pathU pathV Nothing
    where pathTo::Eq a => a -> BinaryTree a -> Maybe [a]
          pathTo x Empty = Nothing
          pathTo x (Node root left right)
            | x == root = Just [x]
            | otherwise = (root:) <$> case (pathTo x left, pathTo x right) of
                          (Nothing, Nothing) -> Nothing
                          (Nothing, result) -> result
                          (result, _) -> result
          leastCommon::Eq a => Maybe [a] -> Maybe [a] -> Maybe a -> Maybe a
          leastCommon Nothing _ _ = Nothing
          leastCommon _ Nothing _ = Nothing
          leastCommon (Just (x:xs)) (Just (y:ys)) prev
            | x == y = leastCommon (Just xs) (Just ys) (Just x)
            | otherwise = prev

data BST t = BSTEmpty | BSTNode t (BST t) (BST t)
    deriving Show

instance Foldable BST where
    foldr :: (a -> b -> b) -> b -> BST a -> b
    foldr _ nv BSTEmpty = nv
    foldr op nv (BSTNode root left right) = foldr op (op root $ foldr op nv right) left

kthSmallest::Ord t => BST t -> Int -> t
kthSmallest = (!!) . foldr (:) []

rangeSearch::Ord t => t -> t -> BST t -> [t]
rangeSearch a b = filter (\x -> x >= a && x <= b) . foldr (:) []

newtype Graph t = Graph [(t, [t])] deriving Show

testGraph :: Graph Int
testGraph = Graph [(1, [2, 3, 4]),
                   (2, [1, 3, 5]),
                   (3, [1, 2, 4, 5]),
                   (4, [1, 3, 5]),
                   (5, [2, 3, 4])]

neighbours::Eq t => t -> Graph t -> [t]
neighbours _ (Graph []) = []
neighbours u (Graph ((v, vs):rest))
    | u == v = vs
    | otherwise = neighbours u (Graph rest)

connectedComponent::Eq t => t -> Graph t -> [t]
connectedComponent u graph = dfs u [u] graph
    where dfs::Eq t => t -> [t] -> Graph t -> [t]
          dfs u visited graph = foldr (\v res -> if v `elem` res
                                                 then res
                                                 else dfs v (v:res) graph
                                      ) visited $ neighbours u graph