{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ComonadTree (main) where

import Core
-- import Data.Monoid
import Prelude
  ( --
    -- Foldable,
    Functor,
    -- Monoid,
    -- foldMap,
    -- mappend,
    Num,
    -- fmap,
    print,
    show,
  )
import qualified Prelude

-- ann
-- left
-- right
-- toString
-- map
-- extend
-- reduce
-- changed

-- Replicating this:
-- https://joneshf.github.io/programming/2015/12/31/Comonads-Monoids-and-Trees.html

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m

data Tree a
  = Branch (Tree a) (Tree a) a
  | Leaf Integer a
  deriving (Show)

newtype Any = Any {getAny :: Bool}
  deriving (Eq, Ord, Show)

instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

-- instance Functor (Tree a) where
--   fmap :: forall a b v t. (a -> b) -> Tree v a -> Tree v b
--   fmap f (Branch l r a) = Branch (fmap f l) (fmap f r) (f a)
--   fmap f (Leaf v a) = Leaf v $ f a

-- extend :: (Tree v a -> b) -> Tree v a -> Tree v b
-- extend f (Branch l r a) =
--   Branch (extend f l) (extend f r) (f (Branch l r a))
-- extend f (Leaf v a) =
--   Leaf v (f $ Leaf v a)

-- reduce :: Tree v Bool -> Bool
-- reduce (Branch l r a) = reduce l && reduce r && a
-- reduce (Leaf v a) = a

-- instance Monoid m => Foldable (Tree m) where
--   -- foldMap :: forall m a v. Monoid m  => (a -> m) -> Tree v a -> m
--   foldMap f (Branch l r a) = foldMap f l `mappend` (foldMap f r) `mappend` f a
--   foldMap f (Leaf v a) = f a

-- changed :: Monoid m => Tree v a -> m
-- changed :: Tree v a -> Any
-- changed tree =
--   foldMap (\a -> Any a) tree

-- leaf1 = Leaf 1 (Changed False)

-- leaf2 = Leaf 2 (Changed False)

-- leaf3 = Leaf 3 (Changed True)

-- branch1 = Branch leaf1 leaf2 (Changed False)

-- branch2 = leaf3

-- branch0 = Branch branch1 branch2 (Changed False)

-- leaf1 = Leaf 1 False

-- leaf2 = Leaf 2 False

-- leaf3 = Leaf 3 True

-- branch1 = Branch leaf1 leaf2 False

-- branch2 = leaf3

-- branch0 = Branch branch1 branch2 False

-- foldMap over `a` so it can be converted to a Monoid
-- data BinaryTree3 v a
--   = Node3 a (BinaryTree3 a v) (BinaryTree3 a v)
--   | Leaf3 v a
--   deriving (Show)

-- instance Foldable (BinaryTree3 a) where
--   foldMap f (Node3 a l r) = foldMap f l `mappend` foldMap f r
--   foldMap f (Leaf3 v a) = f a

-- leaf1 = Leaf3 (1 :: Int) False

-- f1 = foldMap Any leaf1

data BinaryTree3 v a
  = Node3 v a (BinaryTree3 v a) (BinaryTree3 v a)
  | Leaf3 v a
  deriving (Show)

-- newtype DomTree a = BinaryTree Int a

instance Foldable (BinaryTree3 a) where
  foldMap f (Node3 v a l r) = foldMap f l `mappend` foldMap f r `mappend` f a
  foldMap f (Leaf3 v a) = f a

leaf1 = Leaf3 (1 :: Int) False

f1 = foldMap (\a -> Any a) leaf1

-- Node3 (Node3 0 False (Node3 1 False (Leaf3 3 False) (Leaf3 4 False)) (Leaf3 2 True)) False
--   (Node3 (Node3 1 False (Leaf3 3 False) (Leaf3 4 False)) False
--     (Leaf3 (Leaf3 3 False) False)
--     (Leaf3 (Leaf3 4 False) False))
--   (Leaf3 (Leaf3 2 True) True)
duplicate :: BinaryTree3 v a -> BinaryTree3 (BinaryTree3 v a) a
duplicate n@(Node3 v a l r) =
  Node3 n a (duplicate l) (duplicate r)
duplicate l@(Leaf3 v a) =
  Leaf3 l a

extract :: BinaryTree3 v a -> v
extract (Node3 v a l r) = v
extract (Leaf3 v a) = v

fmap :: (v -> w) -> BinaryTree3 v a -> BinaryTree3 w a
fmap f (Node3 v a l r) = Node3 (f v) a (fmap f l) (fmap f r)
fmap f (Leaf3 v a) = Leaf3 (f v) a

extend :: (BinaryTree3 v a -> w) -> BinaryTree3 v a -> BinaryTree3 w a
extend f = fmap f . duplicate

merge [] ys = ys
merge (x : xs) ys = x : merge xs ys

toList :: BinaryTree3 v a -> [v]
toList (Node3 v a l r) =
  [v] `merge` toList l `merge` toList r
toList (Leaf3 v a) = [v]

branch0 =
  Node3
    (0 :: Int)
    False
    ( Node3
        (1 :: Int)
        False
        (Leaf3 (2 :: Int) False)
        (Leaf3 (3 :: Int) False)
    )
    (Leaf3 (4 :: Int) True)

increment (Node3 v a l r) = v + 1
increment (Leaf3 v a) = v + 1

main :: IO ()
main = do
  print "======================"
  -- print $ branch0
  -- print $ reduce branch0
  -- print $ extend changed branch0
  -- print $ getAny $ changed branch0
  -- print $ f1
  print $ toList $ extend increment branch0

-- instance Monoid a => Foldable (Tree a) where
--   foldMap :: forall f m a. Monoid m => (a -> m) -> f a -> m
--   foldMap :: Monoid m => (a -> m) -> f a -> m
--   foldMap f (Branch l r a) = (foldMap f l) `mappend` (foldMap f r) `mappend` f a
--   foldMap f (Leaf v a) = f a

-- foldMap f (Branch l r a) = (foldMap f l) `mappend` (foldMap f r) `mappend` a
-- foldMap f (Leaf v a) = f a

--   -- instance Monoid m => Prelude.Foldable (Tree t m) where
-- foldMap :: forall (t :: * -> *) m a. (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldMap :: (a -> m) -> t a -> m
-- foldMap :: Monoid t =>(a -> Bool) -> Tree t v a -> Bool
-- foldMap f (Branch l r a) = (foldMap f l) `mappend` (foldMap f r) `mappend` a

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldMap f (Branch l r a) = (foldMap f l) `mappend` (foldMap f r) `mappend` a
-- foldMap f (Leaf v a) = f a

-- changed :: Tree t Int Bool -> Bool
-- changed (Branch l r a) = changed l || changed r || a
-- changed (Leaf v a) = a

-- foldMap :: (Monoid m, Foldable f) => m -> f m -> m
-- foldMap f (Branch l r a) = foldMap f l `mappend` foldMap f r `mappend` a
-- foldMap f (Leaf v a) = undefined --f `mappend` a
