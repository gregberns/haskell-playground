-- {-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances,
-- StandaloneDeriving, FlexibleContexts, UndecidableInstances,
-- GADTs, KindSignatures, RankNTypes #-}

module Profunctor.ProgramTwo
    ( programTwo
    ) where

import Prelude hiding (div)
import qualified Data.List as List
import Profunctor.Html as Html

-- https://github.com/joneshf/elm-profunctors/tree/master/src/Buttons/Count

-- type alias Model =
--   Int

-- model : Int -> Model
-- model x =
--   x

-- type alias Msg a b =
--   a -> b

-- update : (a -> b) -> a -> b
-- update msg model =
--   msg model

-- view : Model -> Html.Html a
-- view model =
--   Html.div []
--     [Html.text (toString model)]

-- beginnerProgram : {model : Model, update : Msg Model Model -> Model -> Model, view : Model -> Html.Html a}
-- beginnerProgram =
--   { model = model 0
--   , update = update
--   , view = view
--   }

type Model = Int

type Msg a b = a -> b

model :: Int -> Model
model x = x

update :: (a -> b) -> a -> b
update msg model = msg model

view :: Model -> Html a
view model =
  div []
  [
    text (show model)
  ]

type BasicProgram = (Model,
    Msg Model Model -> Model -> Model,
    Model -> Html Model)

beginnerProgram :: BasicProgram
beginnerProgram =
  ( model 0
  , update
  , view
  )

-- incrementButton : BeginnerProgram (Button.Model (number -> number)) (number -> number)
-- incrementButton :: Program (Model (Msg Model Model)) (Msg Model Model)
-- incrementButton = 
--   beginnerProgram
--   |> model (model (+) (\x -> x + 1))

incrementButton :: Msg Model Model
incrementButton = \x -> x + 1

decrementButton :: Msg Model Model
decrementButton = \x -> x - 1

eval :: BasicProgram
        -> Msg Model Model
        -> Html Model
eval (m, u, v) msg =
  let m2 = (u msg) m in
  -- (v $ m2, m2)
  v m2

programTwo :: IO ()
programTwo = 
  let e = eval beginnerProgram in
  do
    print "Starting..."
    let s1 = e incrementButton
    print $ s1
    
