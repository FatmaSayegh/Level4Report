module Frame exposing (..)

import Html exposing (div, h1, p, text)
import Html.Attributes exposing (..)


main = view "hello there"

view model =
   div [] [ div pageStyle [text "first"]
          , div pageStyle [text "second"]
          , div pageStyle [text "third"]
          ]

pageStyle =
  [ style "height" "100vh"
  ]
