{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core
  ( Eq (..),
    Ord (..),
    Show (..),
    Integral (..),
    RealFrac (..),
    Num (..),
    Fractional (..),
    Bool (..),
    Either (..),
    Ordering (..),
    Int,
    Integer,
    IO,
    Rational,
    seq,
    error,
    undefined,
    const,
    flip,
    curry,
    uncurry,
    id,
    otherwise,
    (.),
    ($),
    (&&),
    (||),
    not,
    even,
    odd,
    fst,
    snd,
    -- , getChar
    -- , on
    -- , first
    -- , second
    -- , (&&&)
    -- , (***)
    -- , IsString(..)
    -- , module Data.Char
    -- , ifThenElse
    -- , bool
  )
where

import Prelude
  ( Bool (..),
    Char,
    Either (..),
    Eq (..),
    Fractional (..),
    IO,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    Ordering (..),
    Rational,
    RealFrac (..),
    Show (..),
    const,
    curry,
    error,
    even,
    flip,
    fst,
    id,
    not,
    odd,
    otherwise,
    seq,
    snd,
    uncurry,
    undefined,
    ($),
    (&&),
    (.),
    (||),
  )