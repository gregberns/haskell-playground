module Profunctor.ProgramThree
    ( programThree
    ) where

import Prelude hiding (div)
import qualified Data.List as List
import Profunctor.Html
import Profunctor.Model (BasicProgram, always, composeModel)

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

-- beginnerProgram : {model : Model, update : Msg Model Model -> Model -> Model, view : Model -> Html.Html a}
basic :: BasicProgram (Model (Int -> Int)) (Msg Int Int)
basic = 
  ( model 0 id
  , update
  , view
  )

-- incrementButton : BeginnerProgram (Button.Model (number -> number)) (number -> number)
incButton :: BasicProgram (Model (Int -> Int)) (Int -> Int)
incButton =
  composeModel (model 1 (\i -> i + 1)) basic

-- decrementButton : BeginnerProgram (Button.Model (number -> number)) (number -> number)
decButton :: BasicProgram (Model (Int -> Int)) (Int -> Int)
decButton =
  composeModel (model 1 (\i -> i - 1)) basic

programThree :: IO ()
programThree = 
  do
    print "Starting..."

