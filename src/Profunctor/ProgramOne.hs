module Profunctor.ProgramOne
    ( programOne
    ) where

import Prelude hiding (div)
import qualified Data.List as List
import Profunctor.Html

-- https://github.com/joneshf/elm-profunctors/tree/master/src/Buttons/Button

-- App
-- beginnerProgram : {model : Model (), update : a -> b -> b, view : Model c -> Html.Html c}
-- beginnerProgram =
--   { model = model "" ()
--   , update = update
--   , view = view
--   }

-- Model
-- type alias Model a =
--   { label : String
--   , msg : a
--   }

-- model : String -> a -> Model a
-- model str msg =
--   {label = str, msg = msg}

-- Update
-- update : a -> b -> b
-- update _ model =
--   model

-- View
-- view : Model a -> Html a
-- view {label, msg} =
--   div []
--     [ button
--         [onClick msg]
--         [text label]
--     ]
-- data List a = []

data Model a = Model String a deriving Show

model :: String -> a -> Model a
model str a = Model str a

update :: a -> b -> b
update _ b = b

view :: Model a -> Html a
view (Model label msg) =
  div []
  [
    button
      [ onClick msg ]
      [ text label ]
  ]

beginnerProgram :: (Model (), a -> b -> b, Model c -> Html c)
beginnerProgram =
  ( model "hi" ()
  , update
  , view
  )

-- Evaluate the program
eval (m, u, v) =
  v $ u () m

programOne :: IO ()
programOne = do
  print $ model "hey" [1]
  print $ eval beginnerProgram
