module Profunctor.Model where

import Profunctor.Html

type BasicProgram model msg =
  ( model
  , msg -> model -> model
  , model -> Html msg
  )

(|>) x f = f x

always :: a -> b -> a
always a _ =
  a

composeModel x =
  mapModel (always x)  

mapModel f (m, u, v) =
  (f m, u, v)

composeUpdate x =
  mapUpdate (always x)

mapUpdate f (m, u, v) =
  (m, f u, v)

composeView x =
  mapView (always x)

mapView f (m, u, v) =
  (m, u, f v)

