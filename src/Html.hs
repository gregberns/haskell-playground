module Html
    ( Html, Attribute,
      onClick, text, div, button
    ) where

import Prelude hiding (div)
import qualified Data.List as List

data Html a = Node String [Attribute a] [Html a] deriving Show

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
