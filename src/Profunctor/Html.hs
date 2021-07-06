module Profunctor.Html
    ( Html, Attribute,
      fmap, onClick, text, div, button
    ) where

import Prelude hiding (div, fmap)
import qualified Data.List as List

data Html a = Node String [Attribute a] [Html a] deriving Show

-- instance Functor (Html a) where
fmap :: (a -> msg) -> Html a -> Html msg
fmap f (Node t as hs) = 
  node t 
    (List.map (\a -> 
      case a of 
        On (s, msg) -> On (s, f msg)
        Attribute (k, v) -> Attribute (k, v) ) as)
    (List.map (Profunctor.Html.fmap f) hs)

data Attribute msg = 
  Attribute (String, String)
  | On (String, msg) deriving Show

node :: String -> [Attribute msg] -> [Html msg] -> Html msg
node tag attrs children = Node tag attrs children

attribute :: String -> String -> Attribute msg
attribute name msg = 
  Attribute (name, msg)

on :: String -> msg -> Attribute msg
on event msg = On (event, msg)

onClick :: msg -> Attribute msg
onClick msg = on "click" msg

text :: String -> Html msg
text s = node "p" [attribute "text" s] []

div :: [Attribute msg] -> [Html msg] -> Html msg
div = node "div"

button :: [Attribute msg] -> [Html msg] -> Html msg
button = node "button" 
