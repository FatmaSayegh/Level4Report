module MyShapes exposing (main)

import Html as H exposing (div, h1, img, text)
import Html.Attributes as HA exposing (class)
import Html.Events exposing (onClick)
import Browser
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)

-- Program starts here, view will take the model in and produce
-- html for you
main = view initialModel


--type Shape = Rectangle | Circle
--type alias Object = {size : Size, shape : Shape, pos : Pos}

initialModel : List Int
initialModel = List.range 1 30

makeObjects : List Int -> List (S.Svg msg)
makeObjects = List.map makeObjectFromInt

makeObjectFromInt : Int -> S.Svg msg
makeObjectFromInt n =
    if (modBy 2 n) == 0 then
        rectangle (n*3) {x = n*50, y = n^2}
    else
        circle ((n*3)//2) {x = n*50, y = n^2}


view model =
    div [ HA.class "content" ]
        [ h1 [] [ H.text "Hello Svg" ]
        , theSvg model
        ]

theSvg model =
    S.svg
     [ SA.width "1200"
     , SA.height "1200"
     , SA.viewBox "0 0 1200 1200"
     ]
     (makeObjects model)

type alias Pos = {x : Int, y : Int}
type alias Size = Int

-- Rectangle takes an Int as size and position as a record. Gives out an Svg
rectangle : Size -> Pos -> S.Svg msg
rectangle size pos =
    S.rect
        [ x (String.fromInt pos.x)
        , y (String.fromInt pos.y)
        , SA.width (String.fromInt size)
        , SA.height (String.fromInt size)
        , SA.style "fill: skyblue;"
        ][]

-- Circle takes an Int as size and position as a record. Gives out an Svg
circle : Size -> Pos -> S.Svg msg
circle size pos =
    S.circle
        [ SA.cx (String.fromInt pos.x)
        , SA.cy (String.fromInt pos.y)
        , SA.r (String.fromInt size)
        , SA.style "fill: red;"
        ]
        []

