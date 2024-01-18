{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
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

data Perhaps a = Yes a | Nope

deriving instance (Show a) => Show (Perhaps a)

safeHead :: [a] -> Perhaps a
safeHead (x:_) = Yes x
safeHead [] = Nope

safeMap :: (a -> Perhaps b) -> [a] -> Perhaps [b]
safeMap _ [] = Yes []
safeMap f (x:xs)
  | (Yes y) <- f x, (Yes ys) <- safeMap f xs = Yes (y:ys)
  | otherwise = Nope

safeMap' :: (a -> Perhaps b) -> [a] -> Perhaps [b]
safeMap' _ [] = Yes []
safeMap' f (x:xs) =
  (safeMap' f xs)   -- :: Perhaps [b]
  >>=
  (\ys -> (\y -> y:ys) <$> f x)   -- :: [b] -> Perhaps [b]

safeMap'' :: (a -> Perhaps b) -> [a] -> Perhaps [b] 
safeMap'' _ [] = pure []
safeMap'' f (x:xs) = do
  ys <- safeMap'' f xs
  y <- f x
  pure (y:ys)

recip' :: Float -> Perhaps Float
recip' 0 = Nope
recip' x = Yes $ 1 / x

instance Functor Perhaps where
  -- fmap :: (a -> b) -> (Perhaps a) -> (Perhaps b)
  fmap _ Nope = Nope
  fmap f (Yes x) = Yes (f x)

instance Applicative Perhaps where
  -- pure :: a -> Perhaps a
  pure x = Yes x

  -- (<*>) :: Perhaps (a -> b) -> Perhaps a -> Perhaps b
  (Yes f) <*> (Yes x) = Yes (f x)
  Nope <*> _ = Nope
  (Yes _) <*> Nope = Nope

instance Monad Perhaps where
  -- (>>=)       :: Perhaps a -> (a -> Perhaps b) -> Perhaps b
  (Yes x) >>= f = f x
  Nope >>= _ = Nope

-- data Maybe a = Just a | Nothing

data State s a = WithState (s -> (a, s))

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (WithState run) = WithState run'
    where
      run' beginState = (f x, oldState)
        where
          (x, oldState) = run beginState

instance Applicative (State s) where
  pure x = WithState (\beginState -> (x, beginState))

  -- runF :: s -> (a -> b, s)
  -- runX :: s -> (a, s)
  -- run  :: s -> (b, s)
  (WithState runF) <*> (WithState runX) = WithState run
    where
      run beginState = (f x, stateAfterX)
        where
          (x, stateAfterX) = runX stateAfterF
          (f, stateAfterF) = runF beginState

instance Monad (State s) where
  -- (>>=)       :: State s a -> (a -> State s b) -> State s b
  (WithState runX) >>= f = WithState run
    where
      run beginState = (y, stateFinal)
        where
          (x, stateAfterX) = runX beginState
          (WithState runFX) = f x
          (y, stateFinal) = runFX stateAfterX

getState :: State s s
getState = WithState run
  where
    run internalState = (internalState, internalState)

setState :: s -> (State s ())
setState newState = (WithState run)
  where
    run _internalState = ((), newState)

data Tree = Tree
  { children :: [Tree]
  , label :: String
  }
  deriving (Show)

myTree :: Tree
myTree = Tree
  { label = "root"
  , children = [
    Tree
      { label = "foo"
      , children = []
      },
    Tree
      { label = "bar"
      , children = [
        Tree
          { label = "qux"
          , children = []
          },
        Tree
          { label = "qox"
          , children = []
          }
        ]
      }
    ]
  }

getLabels :: Tree -> [String]
getLabels (Tree { label, children }) = (concat $ map getLabels children) ++ [label]

addLabelsNW :: Tree -> Tree
addLabelsNW (Tree { label, children }) = Tree
  { label = label ++ "?"
  , children = map addLabelsNW children
  }

runState :: s -> State s a -> a
runState initialState (WithState run) = value
  where
    (value, _endState) = run initialState

addLabels :: Tree -> Tree
addLabels t = runState 0 (addLabels' t)

addLabels' :: Tree -> State Int Tree
addLabels' (Tree { label, children }) = do
  newChildren <- myMapM addLabels' children
  counter <- getState
  let newCounter = counter + 1
  setState newCounter

  pure Tree
    { label = label ++ show counter
    , children = newChildren
    }

addLabelsFromList :: [String] -> Tree -> Tree
addLabelsFromList labels t = runState labels (addLabelsFromList' t)

addLabelsFromList' :: Tree -> State [String] Tree
addLabelsFromList' (Tree { label, children }) = do
  newChildren <- myMapM addLabelsFromList' children
  labels <- getState
  let (newLabel, newLabels) = popLabel labels

  setState newLabels

  pure Tree
    { label = label ++ ":" ++ newLabel
    , children = newChildren
    }

popLabel :: [String] -> (String, [String])
popLabel [] = ("no more labels", [])
popLabel (l:ls) = (l, ls)

myMapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
myMapM _ [] = pure []
myMapM f (x:xs) = do
  y <- f x
  ys <- myMapM f xs
  pure (y:ys)

main :: IO ()
main = do
  name <- getLine
  let str = "hello, " ++ name
  putStrLn str
