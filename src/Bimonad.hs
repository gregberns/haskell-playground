{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Bimonad where

import Prelude
  ( undefined,
    ($),
  )

data Either a b
  = Left a
  | Right b

fmapEither :: (b -> b') -> Either a b -> Either a b'
fmapEither _ (Left a) = Left a
fmapEither f (Right b) = Right $ f b

bimapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimapEither f _ (Left a) = Left $ f a
bimapEither _ g (Right b) = Right $ g b

biflatMapEither :: (a -> Either c d) -> (b -> Either c d) -> Either a b -> Either c d
biflatMapEither f _ (Left a) = f a
biflatMapEither _ g (Right b) = g b

fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple f (a, b) = (a, f b)

bimapTuple :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimapTuple f g (a, b) = (f a, g b)

-- Not sure if this is possible - the return type doesnt look right, and Id ont think theres a way to flatten
-- biflatMapTuple :: (a -> (c,d)) -> (b -> (c,d)) -> (a,b) -> ((c,d), (c,d))
-- biflatMapTuple f g (a, b) =
-- But it is if `a` is a Monoid:
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad.html#control.i:ic:Monad:Monad:25
-- instance Monoid a => Monad ((,) a) where
-- (u, a) >>= k = case k a of (v, b) -> (u `mappend` v, b)
