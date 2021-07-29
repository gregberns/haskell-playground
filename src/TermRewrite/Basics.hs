{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TermRewrite.Basics (main) where

data Bop
  = Add
  | Multi
  | Lte
  | And
  | Or
  deriving (Eq, Show)

data UniOp
  = Not
  deriving (Eq, Show)

newtype Id = Id String
  deriving (Eq, Show)

data AssociativeType
  = AssocLeft
  | AssocRight
  | AssocNone
  deriving (Eq, Show)

data Fun = Fun Args Expr
  deriving (Eq, Show)

data Args = Args [Id]
  deriving (Eq, Show)

data App
  = AppF1 Fun Expr
  | App1 Id Expr
  deriving (Eq, Show)

data Expr
  = Var String
  | Int Int
  | Bool Bool
  | UniOp UniOp Expr
  | BinOp Bop Expr Expr
  | If Expr Expr Expr
  | Let Id Expr Expr
  | FunE Fun
  | AppE App
  | Err String
  deriving (Eq, Show)

data Token
  = TkTrue
  | TkFalse

join sep xs = foldr (\a b -> a ++ if b == "" then b else sep ++ b) "" xs

showToken t =
  case t of
    TkTrue -> "true"
    TkFalse -> "false"

toString = renderExpr

renderExpr (Var s) = s
renderExpr (Int i) = show i
renderExpr (Bool b) = if b then "true" else "false"
renderExpr (UniOp op e) =
  case op of
    Not -> "!" ++ toString e
renderExpr (BinOp op e1 e2) =
  let f o =
        case o of
          Add -> "+"
          Multi -> "*"
          Lte -> "<"
          And -> "&&"
          Or -> "||"
   in toString e1 ++ " " ++ f op ++ " " ++ toString e2
renderExpr (If c t e) =
  "if " ++ toString c ++ " then " ++ toString t ++ " else " ++ toString e
renderExpr (Let (Id i) e1 e2) =
  "let " ++ i ++ " = " ++ toString e1 ++ " in " ++ toString e2
renderExpr (FunE f) =
  renderFun f
renderExpr (AppE a) =
  renderApp a
renderExpr (Err e) = e

renderApp (App1 (Id id) e) = id ++ " " ++ renderExpr e
renderApp (AppF1 f e) = "(" ++ renderFun f ++ ") " ++ renderExpr e

renderFun (Fun args e) = "fun " ++ renderArgs args ++ " -> " ++ renderExpr e

renderArgs (Args l) = join " " $ map (\(Id id) -> id) l

replaceVal v repl expr =
  if expr == v
    then repl
    else case expr of
      BinOp op e1 e2 ->
        BinOp op (replaceVal v repl e1) (replaceVal v repl e2)
      If c t e ->
        If (replaceVal v repl c) (replaceVal v repl t) (replaceVal v repl e)
      Let id e1 e2 ->
        Let id (replaceVal v repl e1) (replaceVal v repl e2)
      e -> e

eval :: Expr -> Expr
eval = evalExpr

evalExpr :: Expr -> Expr
evalExpr e =
  case e of
    Var s -> Var s
    Int i -> Int i
    Bool b -> Bool b
    UniOp op e ->
      case eval e of
        Int i ->
          Err ("Error - Invalid expr: " ++ toString (UniOp op e))
        Bool b ->
          case op of
            Not -> Bool (not b)
        Var v -> UniOp op (Var v)
        i -> Err ("Error - Invalid expr: " ++ toString (UniOp op i))
    BinOp op e1 e2 ->
      case (evalExpr e1, evalExpr e2) of
        (Int i1, Int i2) ->
          case op of
            Add -> Int (i1 + i2)
            Multi -> Int (i1 * i2)
            Lte -> if i1 < i2 then Bool True else Bool False
            _ -> Err ("Error - Invalid expr: " ++ toString (BinOp op e1 e2))
        (Bool b1, Bool b2) ->
          case op of
            And -> Bool (b1 && b2)
            Or -> Bool (b1 || b2)
            _ -> Err ("Error - Invalid expr: " ++ toString (BinOp op e1 e2))
        (Var v1, Var v2) -> BinOp op (Var v1) (Var v2)
        (Var v1, e2) -> BinOp op (Var v1) e2
        (e1, Var v2) -> BinOp op e1 (Var v2)
        (i, j) -> Err ("Error - Invalid expr: " ++ toString (BinOp op i j))
    If c t e ->
      case evalExpr c of
        Bool b -> if b then evalExpr t else evalExpr e
        _ -> Err ("Error - Invalid if clause: " ++ toString (If c t e))
    Let (Id id) e1 e2 ->
      let x = evalExpr e1
       in evalExpr (replaceVal (Var id) x e2)
    FunE f ->
      FunE $ evalFun f
    AppE a ->
      evalApp a
    Err e -> Err e

evalApp :: App -> Expr
evalApp (AppF1 (Fun (Args [(Id id)]) e1) e2) =
  evalExpr $ replaceVal (Var id) e2 e1
evalApp a@(App1 (Id id) e) =
  AppE a

-- Err $ "Arg" ++ show a

-- AppE $ App1 (Id id) $ evalExpr e

evalFun :: Fun -> Fun
evalFun (Fun a e) =
  Fun a (evalExpr e)

assertEqualExpr :: String -> Expr -> Expr -> IO ()
assertEqualExpr msg a b =
  if a == b
    then pure ()
    else do
      putStrLn ("Failed Assert :: (" ++ msg)
      putStrLn ("a: " ++ toString a)
      putStrLn ("a: " ++ toString b)

assertEqualStr :: String -> String -> String -> IO ()
assertEqualStr msg a b =
  if a == b
    then pure ()
    else do
      putStrLn ("Failed Assert :: (" ++ msg ++ ")")
      putStrLn ("a: " ++ a)
      putStrLn ("b: " ++ b)

runTests :: IO ()
runTests =
  do
    let e =
          Let
            (Id "inc")
            (FunE $ Fun (Args [(Id "x")]) (BinOp Add (Var "x") (Int 1)))
            (AppE $ App1 (Id "inc") (Int 1))
     in do
          assertEqualStr "let inc = fun x -> x + 1 in inc 1" "let inc = fun x -> x + 1 in inc 1" (toString e)
          assertEqualStr "eval let inc = fun x -> x + 1 in inc 1" "let inc = fun x -> x + 1 in inc 1" (toString (eval e))

    let e =
          (AppE $ AppF1 (Fun (Args [(Id "x")]) (BinOp Add (Var "x") (Int 1))) (Int 1))
     in do
          assertEqualStr "(fun x -> x + 1) 1" "(fun x -> x + 1) 1" (toString e)
          assertEqualStr "eval (fun x -> x + 1) 1" "2" (toString (eval e))

    let e = UniOp Not (Bool True)
     in do
          assertEqualStr "!true" "!true" (toString e)
          assertEqualStr "!true" "false" (toString (eval e))

    -- (* !true && true *)
    let e = BinOp And (UniOp Not (Bool True)) (Bool True)
     in do
          assertEqualStr "!true && true" "!true && true" (toString e)
          assertEqualStr "eval !true && true" "false" (toString (eval e))

    -- (* replace_val v repl expr *)
    let e = BinOp And (Var "x") (Bool True)
     in assertEqualStr "replace_val true && true" "true && true" (toString (replaceVal (Var "x") (Bool True) e))

    -- (* x && y || z *)
    let e = BinOp Or (BinOp And (Var "x") (Var "y")) (Var "z")
     in do
          assertEqualStr "x && y || z" "x && y || z" (toString e)
          assertEqualStr "eval x && y || z" "x && y || z" (toString (eval e))

    -- (* let x = true in x *)
    let e = Let (Id "x") (Bool True) (Var "x")
     in do
          assertEqualStr "let x = true in x" "let x = true in x" (toString e)
          assertEqualStr "eval let x = true in x" "true" (toString (eval e))

    -- (* let x = 1 + 4 in x * 3  *)
    let e = Let (Id "x") (BinOp Add (Int 1) (Int 4)) (BinOp Multi (Var "x") (Int 3))
     in do
          assertEqualStr "let x = 1 + 4 in x * 3" "let x = 1 + 4 in x * 3" (toString e)
          assertEqualStr "eval let x = 1 + 4 in x * 3" "15" (toString (eval e))

    -- (* let x = true in let y = false in x && y *)
    let e = Let (Id "x") (Bool True) (Let (Id "y") (Bool False) (BinOp And (Var "x") (Var "y")))
     in do
          assertEqualStr "let x = true in let y = false in x && y" "let x = true in let y = false in x && y" (toString e)
          assertEqualStr "eval let x = true in let y = false in x && y" "false" (toString (eval e))

    -- (* if true && true then true || false else false && false *)
    let e = If (BinOp And (Bool True) (Bool True)) (BinOp Or (Bool True) (Bool False)) (BinOp And (Bool False) (Bool False))
     in do
          assertEqualStr
            "if true && true then true || false else false && false"
            "if true && true then true || false else false && false"
            (toString e)
          assertEqualStr "eval if true && true then true || false else false && false" "true" (toString (eval e))

    -- (* if true then true else false *)
    let e = If (Bool True) (Bool True) (Bool False)
     in do
          assertEqualStr "if true then true else false" "if true then true else false" (toString e)
          assertEqualStr "eval if true then true else false" "true" (toString (eval e))

    -- (* if false then true else false *)
    let e = If (Bool False) (Bool True) (Bool False)
     in do
          assertEqualStr "if false then true else false" "if false then true else false" (toString e)
          assertEqualStr "eval if false then true else false" "false" (toString (eval e))

    -- (* if 1 then 1 else 2 *)
    let e = If (Int 1) (Bool True) (Bool False)
     in do
          assertEqualStr "if 1 then true else false" "if 1 then true else false" (toString e)
          assertEqualStr "eval if 1 then true else false" "Error - Invalid if clause: if 1 then true else false" (toString (eval e))

    -- (* if false then 1 else 2 *)
    let e = If (Bool True) (Int 1) (Int 2)
     in do
          assertEqualStr "if true then 1 else 2" "if true then 1 else 2" (toString e)
          assertEqualStr "eval if true then 1 else 2" "1" (toString (eval e))

    -- (* true && true *)
    let e = BinOp And (Bool True) (Bool True)
     in do
          assertEqualStr "true and true" "true && true" (toString e)
          assertEqualStr "eval true and true" "true" (toString (eval e))

    -- (* true && true && true *)
    let e = BinOp And (Bool True) (BinOp And (Bool True) (Bool True))
     in do
          assertEqualStr "true and true and true" "true && true && true" (toString e)
          assertEqualStr "eval true and true and true" "true" (toString (eval e))

    -- (* false || true *)
    let e = BinOp Or (Bool False) (Bool True)
     in do
          assertEqualStr "false or true" "false || true" (toString e)
          assertEqualStr "eval false or true" "true" (toString (eval e))

    -- (* false || false || true *)
    let e = BinOp Or (Bool False) (BinOp Or (Bool False) (Bool True))
     in do
          assertEqualStr "false or false or true" "false || false || true" (toString e)
          assertEqualStr "eval false or false or true" "true" (toString (eval e))

    -- (* Error: 1 + true *)
    let e = BinOp Add (Int 1) (Bool True)
     in do
          assertEqualStr "1 + true" "1 + true" (toString e)
          assertEqualStr "eval 1 + true" "Error - Invalid expr: 1 + true" (toString (eval e))

    -- (* 1 + 2 *)
    let e = BinOp Add (Int 1) (Int 2)
     in do
          assertEqualStr "1 + 2" "1 + 2" (toString e)
          assertEqualStr "eval 1 + 2" "3" (toString (eval e))

    -- (* 1 < 2 *)
    let e = BinOp Lte (Int 1) (Int 2)
     in do
          assertEqualStr "1 < 2" "1 < 2" (toString e)
          assertEqualStr "eval 1 < 2" "true" (toString (eval e))

main = runTests