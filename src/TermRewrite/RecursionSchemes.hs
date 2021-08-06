{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

module TermRewrite.RecursionSchemes where

-- https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

import Control.Arrow

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data Expr
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary String Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Lit
  deriving (Show, Eq)

data Stmt
  = Break
  | Continue
  | Empty
  | IfElse Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | While Expr [Stmt]
  | Expression Expr
  deriving (Show, Eq)

applyExpr :: (Expr -> Expr) -> Expr -> Expr
-- base case: applyExpr is the identity function on constants
applyExpr f (Literal i) = Literal i
-- recursive cases: apply f to each subexpression
applyExpr f (Paren p) = Paren (f p)
applyExpr f (Index e i) = Index (f e) (f i)
applyExpr f (Call e args) = Call (f e) (map f args)
applyExpr f (Unary op arg) = Unary op (f arg)
applyExpr f (Binary l op r) = Binary (f l) op (f r)

flatten' :: Expr -> Expr
flatten' (Paren e) = flatten' e
flatten' x = applyExpr flatten' x

-- INPUT:  (((anArray[(10)])))
-- OUTPUT: anArray[10]
theExpr = Paren (Paren (Paren (Index (Literal (Ident "anArray")) (Paren (Literal (IntLit 10))))))

data ExprF a
  = IndexF a a
  | CallF [a]
  | UnaryF String a
  | BinaryF a String a
  | ParenF a
  | LiteralF Lit
  deriving (Show, Eq, Functor)

-- apply :: (a -> b) -> ExprF a -> ExprF b

-- :set -XTypeApplications
-- :t In @ExprF
-- In @ExprF :: ExprF (Term ExprF) -> Term ExprF

-- data Term f = In (f (Term f))

-- out :: Term f -> f (Term f)
-- out (In t) = t

newtype Term f = In {out :: f (Term f)}

-- a type synonym
type Expr' = Term ExprF

topDown, bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
  out -- 1) unpack
    >>> fmap (bottomUp fn) -- 2) recurse
    >>> In -- 3) repack
    >>> fn -- 4) apply
topDown fn =
  fn -- Apply ƒ to the term.
    >>> out -- Unpack the term so as to access its children.
    >>> fmap (topDown fn) -- Recursively traverse each child of the term with ƒ.
    >>> In -- Repack the term.

flattenTerm :: Expr' -> Expr'
flattenTerm (In (ParenF e)) = e
flattenTerm other = other

flatten'' :: Expr' -> Expr'
flatten'' = bottomUp flattenTerm

runTests = do
  show $ flatten' theExpr

main = runTests