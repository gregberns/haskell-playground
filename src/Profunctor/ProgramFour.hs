module Profunctor.ProgramFour
    ( programFour
    ) where

import Prelude hiding (div)
import qualified Data.List as List
import Data.Bifunctor (bimap)
import Profunctor.Html as Html
import Profunctor.Model as Model
  (BasicProgram, (|>), always, composeModel, 
  composeUpdate, composeView, mapView)

-- https://github.com/joneshf/elm-profunctors/tree/master/src/Buttons/Count

type Msg = Int -> Int

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
basic :: BasicProgram (Model Msg) Msg
basic = 
  ( model 0 id
  , update
  , view
  )

-- incrementButton : BeginnerProgram (Button.Model (number -> number)) (number -> number)
incrementButton :: BasicProgram (Model Msg) Msg
incrementButton =
  composeModel (model 1 (\i -> i + 1)) basic

-- decrementButton : BeginnerProgram (Button.Model (number -> number)) (number -> number)
decrementButton :: BasicProgram (Model Msg) Msg
decrementButton =
  composeModel (model 1 (\i -> i - 1)) basic

type Composition model1 msg1 model2 msg2 model msg =
  BasicProgram model1 msg1 -> BasicProgram model2 msg2 -> BasicProgram model msg

above :: Composition c d a b (a, c) (Either b d)
above second first =
  below first second

below :: Composition a b c d (a, c) (Either b d)
below (m1, u1, v1) (m2, u2, v2) =
  ( (m1, m2)
  , \firstRightMsg (firstModel, secondModel) ->
      case firstRightMsg of
        Left msg ->
          (u1 msg firstModel, secondModel)
        Right msg ->
          (firstModel, u2 msg secondModel)
  , \(firstModel, secondModel) ->
      div []
        [ Html.fmap Left (v1 firstModel)
        , Html.fmap Right (v2 secondModel)
        ]
  )

-- shareMsg
--   ::  Composition c b a b (a, c) (Either b b)
--   -> Composition c b a b (a, c) b
-- shareMsg composition (m2, u2, v2) (m1, u1, v1) =
--   composition (m2, u2, v2) (m1, u1, v1)
--     |> composeUpdate (\msg (firstModel, secondModel) ->
--         (u1 msg firstModel, u2 msg secondModel)
--       )
--     |> mapView ((.) (Html.fmap (bimap id id)))

-- main =
--   decrementButton
--     |> shareMsg above Count.beginnerProgram
--     |> shareMsg above incrementButton
--     |> Html.App.beginnerProgram
-- main :: BasicProgram (Model Msg) Msg
-- main = 
--   decrementButton
--   |> shareMsg above basic
--   |> shareMsg above incrementButton
  -- |> Html.App.beginnerProgram

programFour :: IO ()
programFour = 
  do
    print "Starting..."

