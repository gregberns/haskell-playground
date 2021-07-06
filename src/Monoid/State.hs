-- Look at State and Store
-- http://comonad.com/reader/2018/the-state-comonad/
-- The streams code above suggests at least one kind of use-case, something like merging together changes of position in a stream, analogous to the “zipping monad” you have on infinite streams. But now the positions aren’t just Integers, they are arbitrary values taken from any monoid you want. What other kind of spaces might we want to “zip” in this manner?
-- “Monitor position in stream - or where you are in a  list?

module Monoid.State where

import Prelude hiding (fmap, pure)
-- https://acm.wustl.edu/functional/state-monad.php

instance Show (a -> b) where
  show _ = "<function>"

data State s a = State { runState :: s -> (a, s) } deriving Show

fmap :: (a -> b) -> (State s a) -> (State s b)
fmap f (State g) = State $ 
  (\s -> 
    let (a, s') = g s
    in (f a, s')
  )

pure :: a -> State s a
pure a = State (\s -> (a, s))

-- Cant figure this out
-- bind :: State s a -> (a -> State s b) -> State s b
-- bind (State m) k = 
--   State (\s -> 
--     let (a, s') = m s
--     in k a
--   )

addThree = fmap (\a -> a + 3) (pure 1)

programState :: IO ()
programState = do
  print "====State===="
  print $ runState addThree "a"
