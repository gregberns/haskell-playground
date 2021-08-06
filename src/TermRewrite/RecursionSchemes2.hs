{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

module TermRewrite.RecursionSchemes2 where

-- https://blog.sumtypeofway.com/posts/recursion-schemes-part-2.html

-- :set -package pretty

import Control.Arrow

-- import Text.PrettyPrint (Doc)
-- import qualified Text.PrettyPrint as P

-- prettyPrint :: Algebra Expr Doc

data Expr a
  = Literal {intVal :: Int}
  | Ident {name :: String}
  | Index {target :: a, idx :: a}
  | Unary {op :: String, target :: a}
  | Binary {lhs :: a, op :: String, rhs :: a}
  | Call {func :: a, args :: [a]}
  | Paren {target :: a}
  deriving (Show, Eq, Functor)

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
  out ---1) unpack a `Term a` into an `a (Term a)`
    >>> fmap (bottomUp fn) ---2) recurse, with fn, into the subterms
    >>> In ---3) repack the `a (Term a)` into a `Term a`
    >>> fn ---4) finally, apply fn to the packed `Term a`

ten, add, call :: Term Expr
ten = In (Literal {intVal = 10})
add = In (Ident {name = "add"})
call = In (Call {func = add, args = [ten, ten]}) --add(10, 10)

newtype Term f = In {out :: f (Term f)}

mystery :: Functor f => (f a -> a) -> Term f -> a
mystery fn =
  out --- f (Expr Int)
    >>> fmap (mystery fn) -- IN: f (Expr Int)   OUT: f (Expr Int)
    --- ^ "fmap mystery is the identity function over Literal and Ident values, as they do not contain any subexpressions."
    --- ^ I think this will only end up working on leafs - it recurses until it can't find any more?
    >>> fn ---3) apply `fn`
    --- ^ I think this will work on nodes

countNodes :: Expr Int -> Int
countNodes (Unary _ arg) = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args) = fn + sum args + 1
countNodes (Index it idx) = it + idx + 1
countNodes (Paren arg) = arg + 1
countNodes (Literal _) = 1
countNodes (Ident _) = 1

-- > mystery countNodes call
-- 4

type Algebra f a = f a -> a

cata :: (Functor f) => (f a -> a) -> Term f -> a
-- cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

-- bottomUp f is just cata f, with the additional step of stuffing the accumulator value into a Term with In before handing it off to f:
bottomUp' f = cata (In >>> f)

-- Catamorphisms can also fuse:
-- given alg :: f a -> a
-- and func  :: f a -> f
-- cata (alg >>> fmap func) ≍ (cata alg) >>> func
-- cata ((f a -> a) >>> fmap (f a -> f)) ≍ (cata (f a -> a)) >>> (f a -> f)

-- Catamorphisms also compose: given an algebra over a given type
-- given alg  :: f a -> a
-- and func :: f a -> g a
-- cata (f >>> In) >>> cata g ≍ cata (f >>> g)

-- bottomUp f = out >>> fmap (bottomUp f) >>> In  >>> f
-- topDown  f = f >>> out >>> fmap (topDown f) >>> In

-- cata' f = out >>> fmap (cata f) >>> f
reversed :: Functor f => (a -> f a) -> a -> Term f
reversed f = In <<< fmap (reversed f) <<< f

type Coalgebra f a = a -> f a

-- Anamorphism – the “ana” prefix means “building”
-- Just like cata generalized fold from lists to any Functor, ana generalizes unfold to any Functor.

-- If we thought about algebras as reunions,
-- we can think about coalgebras as disassembly or dispersion.

-- `m a -> (a -> m b) -> m b` - NOT a coalgebra

ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

nested :: Int -> Term Expr
nested n = ana go n
  where
    go :: Coalgebra Expr Int
    go 0 = Literal n
    go n = Paren (n - 1)