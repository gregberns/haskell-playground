{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- http://comonad.com/reader/2012/abstracting-with-applicatives/

module Appl where

import Control.Applicative hiding (Const)
import Control.Monad.Identity
import Data.Monoid hiding (Product, Sum)

-- instance Show a => Show (Identity a) where
--     show (Identity x) = "(Identity " ++ show x ++ ")"

data Const mo a = Const mo deriving (Show)

instance Functor (Const mo) where
  fmap _ (Const mo) = Const mo

instance Monoid mo => Applicative (Const mo) where
  pure _ = Const mempty
  (Const f) <*> (Const x) = Const (f <> x)

newtype Compose f g a = Compose (f (g a)) deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose $ (fmap . fmap) f x

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose f) <*> (Compose x) = Compose $ (<*>) <$> f <*> x

data Product f g a = Product (f a) (g a) deriving (Show)

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Product x y) = Product (fmap f x) (fmap f y)

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure x = Product (pure x) (pure x)
  (Product f g) <*> (Product x y) = Product (f <*> x) (g <*> y)

data Tree v a
  = Leaf v a
  | Branch v (Tree v a) (Tree v a)

toList :: Tree v a -> [a]
toList (Leaf _ a) = [a]
toList (Branch _ x y) = toList x ++ toList y

tag :: Tree v a -> v
tag (Leaf v _) = v
tag (Branch v _ _) = v

head :: Tree v a -> a
head (Leaf _ a) = a
head (Branch _ x y) = Appl.head x

walkTree =
  let a = Leaf 1 "a"
   in let b = Leaf 2 "b"
       in let c = Branch 3 a b
           in do
                print $ toList c
                print $ tag c
                print $ Appl.head c

something =
  let a = Const "1"
   in let b = Const "2"
       in a <*> b

type Writer mo = Product (Const mo) Identity

tell :: mo -> Writer mo ()
tell x = Product (Const x) (pure ())

doit = tell [1] *> tell [2]

type FailingWriter mo = Compose (Writer mo) Maybe

tellFW :: Monoid mo => mo -> FailingWriter mo ()
tellFW x = Compose (tell x *> pure (Just ()))

failFW :: Monoid mo => FailingWriter mo a
failFW = Compose (pure Nothing)

doitF = tellFW [1] *> failFW *> tellFW [2]

-- runit :: Compose (Writer [[Char]]) Maybe () -> Compose (Writer [Char]) Maybe ()
-- runit (Compose (Product w m)) =
-- let s = (\cs -> foldr (\ag i -> ag ++ i) "" cs) <$> w in
-- Compose (s) m

main :: IO ()
-- main = putStrLn "some"
main = do
  -- print doit
  -- print $ tellFW [1] *> tellFW [2]
  -- print $ tellFW [1] *> failFW *> tellFW [2]
  -- print $ tellFW ["Hi"] *> tellFW ["Chica"]
  -- print $ tellFW ["Buenas"] *> failFW *> tellFW ["Dia !"]
  -- print $ something
  -- print $ runit $ tellFW ["Hi"] *> tellFW ["Chica"]
  walkTree