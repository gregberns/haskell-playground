{-# LANGUAGE GADTs #-}

module Monoid.Cata where

-- https://www.youtube.com/watch?v=-bHU7fPUP6E

data Fold a b = forall m. Monoid m => Fold (a -> m) (m -> b)

fold :: Fold a b -> [a] -> b
fold (Fold toM fromM) = fromM . mconcat . fmap toM

instance Functor (Fold a) where
  fmap f (Fold toM fromM) = Fold toM (f . fromM)

class Monoidal f where
  init :: f ()
  combine :: f a -> f b -> f (a, b)

isEven :: Int -> Bool
isEven x = (x `mod` 2) == 0

toM :: Int -> [Int]
toM x =
  case isEven x of
  True -> [x]
  False -> []

fromM :: [Int] -> Int
fromM [] = 0
fromM (x : xs) = x + (fromM xs)

list :: [Int]
list = [1,2,3,4]

output = fold (Fold toM fromM) list

-- This is not done ---- Just got the basic fold to work


cata :: IO ()
cata = do
  print $ "=====Cata Start====="
  print $ output
