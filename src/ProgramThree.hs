module ProgramThree
    ( programThree
    ) where

import Prelude hiding (div)
import qualified Data.List as List
import Html

-- https://github.com/joneshf/elm-profunctors/tree/master/src/Buttons/Count

type Msg a b = a -> b

type Model a = (Int, a)
model :: Int -> a -> Model a
model i msg = (i, msg)

-- update :: (a -> b) -> a -> b
update :: (Int -> Int) -> Model a -> Model a
update msg (i, a) = model (msg i) a

view :: Model a -> Html a
view (i, a) =
  div []
  [
    text (show i)
  ]

type BasicProgram model msg =
  ( model
  , msg -> model -> model
  , model -> Html msg
  )
basic :: BasicProgram (Model (Int -> Int)) (Int -> Int)
basic = 
  ( model 0 id
  , update
  , view
  )

always :: a -> b -> a
always a _ =
  a

doModel x =
  mapModel (always x)  

mapModel f (m, u, v) =
  (f m, u, v)

-- incrementButton : BeginnerProgram (Button.Model (number -> number)) (number -> number)
incButton :: BasicProgram (Model (Int -> Int)) (Int -> Int)
incButton =
  doModel (model 1 (\i -> i + 1)) basic

programThree :: IO ()
programThree = 
  do
    print "Starting..."
