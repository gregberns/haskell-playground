{-# LANGUAGE KindSignatures #-}

module Profunctor.Basics where

-- https://typeclasses.com/profunctors

-- Profunctors are bifunctors that are contravariant in their
-- first type argument and covariant in their second one.
-- Make sure that you understand contravariance first.
-- Then we just need to talk about bifunctors, and
-- finally we will get to profunctors.

class Bifunctor (f :: * -> * -> *) where
  bimap ::
    (a0 -> z0) ->
    (a1 -> z1) ->
    f a0 a1 ->
    f z0 z1

instance Bifunctor ((,)) where
  bimap f g (x, y) = (f x, g y)

greet ::
  Bifunctor p =>
  p String String ->
  p String String
greet = bimap ("hello " ++) ("goodbye " ++)

programProfunctorBasics :: IO ()
programProfunctorBasics = do
  print "====Profunctor.Basics===="
  print $ greet ("a", "b")

class Profunctor (f :: * -> * -> *) where
  dimap ::
    (a1 -> a0) ->
    (z0 -> z1) ->
    f a0 z0 ->
    f a1 z1

-- instance Profunctor ((,)) where
--   dimap f g (x, y) = (  , (g y))
