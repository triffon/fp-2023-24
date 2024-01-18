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

data State a = WithState (Int -> (a, Int))

instance Functor State where
  -- fmap :: (a -> b) -> State a -> State b
  fmap f (WithState run) = WithState run'
    where
      run' beginState = (f x, oldState)
        where
          (x, oldState) = run beginState

instance Applicative State where
  pure x = WithState (\beginState -> (x, beginState))

  -- runF :: Int -> (a -> b, Int)
  -- runX :: Int -> (a, Int)
  -- run  :: Int -> (b, Int)
  (WithState runF) <*> (WithState runX) = WithState run
    where
      run beginState = (f x, stateAfterX)
        where
          (x, stateAfterX) = runX stateAfterF
          (f, stateAfterF) = runF beginState

instance Monad State where
  -- (>>=)       :: State a -> (a -> State b) -> State b
  (WithState runX) >>= f = WithState run
    where
      run beginState = (y, stateFinal)
        where
          (x, stateAfterX) = runX beginState
          (WithState runFX) = f x
          (y, stateFinal) = runFX stateAfterX
