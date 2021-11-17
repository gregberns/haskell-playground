{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module MonadControl where

import Control.Monad.Error
import Data.Functor.Identity
import GHC.IO.Handle
import System.IO

-- Monad Control
-- https://www.yesodweb.com/book/monad-control
-- Monad Transformers: allow you to take different pieces of functionality - such as mutable state, error handling, or logging- and compose them together easily.

-- We have the core monad- also known as the innermost or bottom monad. On top of this core, we add layers, each adding a new feature and spreading outward/upward.

-- newtype ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}

-- class MonadTrans t where
--     lift :: Monad m => m a -> t m a

-- instance (Error e) => MonadTrans (ErrorT e) where
--   lift m = ErrorT $ do
--       a <- m
--       return (Right a)
type MyError = String

type MyStack = ErrorT MyError IO

type ErrorTUnwrapped e m a = m (Either e a)

sayHi' :: IO ()
sayHi' = putStrLn "Hello"

sayHiError' :: ErrorT MyError IO ()
sayHiError' = lift $ putStrLn "Hello"

withMyFile :: (Handle -> IO a) -> IO a
withMyFile = withFile "test.txt" WriteMode

sayHi :: Handle -> IO ()
sayHi handle = hPutStrLn handle "Hi there"

useMyFile :: IO ()
useMyFile = withMyFile sayHi

sayHiError :: Handle -> ErrorT MyError IO ()
sayHiError handle = do
  lift $ hPutStrLn handle "Hi there, error!"
  throwError MyError

-- Results in an expected error
-- useMyFileErrorBad :: ErrorT MyError IO ()
-- useMyFileErrorBad = withMyFile sayHiError

useMyFileError1 :: ErrorT MyError IO ()
useMyFileError1 =
  let unwrapped :: Handle -> IO (Either MyError ())
      unwrapped handle = runErrorT $ sayHiError handle
      applied :: IO (Either MyError ())
      applied = withMyFile unwrapped
      rewrapped :: ErrorT MyError IO ()
      rewrapped = ErrorT applied
   in rewrapped

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

newtype WriterT w m a = WriterT {unWriterT :: w -> m (a, w)}

-- The only "input" datatype to this thing is t, a monad transformer.
-- The left hand side, t n b, is our monad transformer wrapped around the n monad and holding a b value
--   Ex.  MyTrans FirstMonad MyValue
-- It then returns a value with the transformer "popped" inside, with a brand new monad at its core.
--  Ex.   FirstMonad (MyTrans NewMonad MyValue)
-- This is just like:
-- newtype ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}
type Run t = forall n o b. (Monad n, Monad o, Monad (t o)) => t n b -> n (t o b)

errorRun :: Run (ErrorT MyError)
errorRun = undefined

useMyFileError2 :: IO (ErrorT MyError Identity ())
useMyFileError2 =
  let afterRun :: Handle -> IO (ErrorT MyError Identity ())
      afterRun handle = errorRun $ sayHiError handle
      applied :: IO (ErrorT MyError Identity ())
      applied = withMyFile afterRun
   in applied

class MonadTrans t => MonadTransControl t where
  liftControl :: Monad m => (Run t -> m a) -> t m a

useMyFileError3 :: Monad m => ErrorT MyError IO (ErrorT MyError m ())
useMyFileError3 =
  liftControl inside
  where
    inside :: Monad m => Run (ErrorT MyError) -> IO (ErrorT MyError m ())
    inside run = withMyFile $ helper run
    helper ::
      Monad m =>
      Run (ErrorT MyError) ->
      Handle ->
      IO (ErrorT MyError m ())
    helper run handle = run (sayHiError handle :: ErrorT MyError IO ())

useMyFileError4 :: ErrorT MyError IO ()
useMyFileError4 = join useMyFileError3

control ::
  (Monad m, Monad (t m), MonadTransControl t) =>
  (Run t -> m (t m a)) ->
  t m a
control = join . liftControl

useMyFileError5 :: ErrorT MyError IO ()
useMyFileError5 =
  control inside
  where
    inside :: Monad m => Run (ErrorT MyError) -> IO (ErrorT MyError m ())
    inside run = withMyFile $ helper run
    helper ::
      Monad m =>
      Run (ErrorT MyError) ->
      Handle ->
      IO (ErrorT MyError m ())
    helper run handle = run (sayHiError handle :: ErrorT MyError IO ())

useMyFileError6 :: ErrorT MyError IO ()
useMyFileError6 = control $ \run -> withMyFile $ run . sayHiError

-- RunInBase (ErrorT MyError IO) IO = forall b. ErrorT MyError IO b -> IO (ErrorT MyError IO b)
type RunInBase m base = forall b. m b -> base (m b)

class MonadIO m => MonadControlIO m where
  liftControlIO :: (RunInBase m IO -> IO a) -> m a

controlIO :: MonadControlIO m => (RunInBase m IO -> IO (m a)) -> m a
controlIO = join . liftControlIO
