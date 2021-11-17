{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Bimonad where

-- The following was an attempt to understand how logging might be done within ReasonML using
-- a Writer and IO a' e' as the internal monad and a Monoid in the Writer
-- There was a thought that a Bimonad was needed - and Bi, meaning similar to a Bifunctor,
-- not as in BiMonad which is a Monad and Comonad.

-- Additionally explored is the idea of using MonadControl (from Haskell), which uses a Monad Transformer
-- to wrap and unwrap the monads where necessary.
-- SEE: MonadControl
--  Resources (in order of discovery)
-- https://hackage.haskell.org/package/logging
-- https://github.com/jwiegley/logging/blob/master/Control/Logging.hs
-- https://hackage.haskell.org/package/monad-control-1.0.3.1/docs/Control-Monad-Trans-Control.html#t:MonadBaseControl
-- https://www.yesodweb.com/book/monad-control

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

-- https://stackoverflow.com/questions/13556314/biapplicative-and-bimonad
-- bireturn :: (a -> l a b, b -> r a b)
-- bijoin :: (l (l a b) (r a b) -> l a b, r (l a b) (r a b) -> r a b)

-- bibindl :: l a b -> (a -> l c d) -> (b -> r c d) -> l c d
-- bibindl lab l r = bijoinl (bimap l r lab)
-- bibindr :: r a b -> (a -> l c d) -> (b -> r c d) -> r c d
-- bibindr rab l r = bijoinr (bimap l r rab)

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
