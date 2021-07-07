{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module ComonadStoreUi (main) where

import Control.Monad
-- import Control.Comonad
-- import Control.Comonad.Store
import Prelude
  ( Functor,
    IO,
    Int,
    Num,
    Show,
    print,
    show,
    ($),
    (+),
    (.),
  )
import qualified Prelude

-- https://functorial.com/the-future-is-comonadic/main.pdf
data Store s a = Store
  { here :: s,
    view :: s -> a
  }

instance Functor (Store a) where
  fmap f (Store here view) = Store here (f . view)

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b

instance Comonad (Store s) where
  extract (Store here view) =
    view here
  duplicate (Store here view) =
    Store here (\next -> Store next view)
  extend f =
    fmap f . duplicate

-- http://comonad.com/reader/2011/monads-from-comonads/
newtype Co w a = Co {runCo :: forall r. w (a -> r) -> r}

instance Functor w => Functor (Co w) where
  fmap f (Co w) = Co (w . fmap (. f))

instance Comonad w => Prelude.Applicative (Co w) where
  mf <*> ma = mf >>= \f -> fmap f ma
  pure a = Co (`extract` a)

instance Comonad w => Monad (Co w) where
  return a = Co (`extract` a)
  Co k >>= f = Co (k . extend (\wa a -> runCo (f a) wa))

-- instance Comonad w => Monad (Co w) where
select :: Comonad w => Co w (a -> b) -> w a -> w b
select co w =
  runCo co (extend dist w)
  where
    dist fs f = fmap (f $) fs

move :: s -> Store s a -> Store s a
move s store = view (duplicate store) s

main :: IO ()
main = do
  print "======================"

-- print $ extract s1
-- print $ experiment (\s -> [s]) s1
-- print $ extract $ extend (\(Store f a) -> a + 3) s1
