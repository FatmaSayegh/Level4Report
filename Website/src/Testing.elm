module Test exposing (..)

import Browser
import Element as ELE
import Html as H exposing (Html)
import Browser.Navigation as Nav
import Url


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscription
        }

init : () -> ( Model, Cmd Msg )
init _ =
   ({ width = 23
    , height = 23
    }
   , Cmd.none)

type alias Model =
   { width : Int
   , height : Int
   }
type Msg =
   Hello
   | Bye

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
   case msg of
      Hello ->
         (model, Cmd.none)
      Bye ->
         (model, Cmd.none)
      

subscription : Model -> Sub Msg
subscription _ =
    Sub.batch
      []

layOutAttributes = 
                   [ ELE.height ELE.fill
                   , ELE.padding 30
                   ]

view : Model -> Html Msg
view model =
   ELE.layout
      layOutAttributes 
      <| ELE.el [] (ELE.text <| String.fromInt model.width)
