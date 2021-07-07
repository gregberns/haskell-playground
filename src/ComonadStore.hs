{-# LANGUAGE DeriveFunctor #-}

module ComonadStore (main) where

import Control.Comonad
import Prelude
  ( -- Functor,
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

-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/cellular-automata

-- A `Store s a` takes a configuration `s` and will produce a value of type `a`
data Store s a = Store (s -> a) s deriving (Functor)

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

fmap :: (a -> b) -> Store s a -> Store s b
fmap f (Store g a) = Store (f . g) a

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment k (Store f s) = f <$> k s

s1 = Store ((+) 1) (1 :: Int)

main :: IO ()
main = do
  print "======================"
  -- print $ extract s1
  -- print $ experiment (\s -> [s]) s1
  print $ extract $ extend (\(Store f a) -> a + 3) s1
