module Bounce exposing (main)

import Browser
import Browser.Events as E
import Html as H exposing (div, h1, img, text)
import Html.Attributes as HA exposing (class)
import Html.Events
import Math.Vector2 exposing (Vec2, add, dot, getX, getY, scale, vec2)
import Shapes
    exposing
        ( Object
        , Pos
        , Shape(..)
        , Size
        , makeCanvas
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscription
        }


type alias Model =
    { canvasWidth : Int
    , canvasHeight : Int
    , kinematicObjects : List KinematicObject
    }


type alias KinematicObject =
    { position : Vec2
    , velocity : Vec2
    , acceleration : Vec2
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        listOfObs =
            [ { position = vec2 50 50
              , velocity = vec2 500 600
              , acceleration = vec2 0 200
              }
            , { position = vec2 600 100
              , velocity = vec2 -600 800
              , acceleration = vec2 0 200
              }
            , { position = vec2 1000 300
              , velocity = vec2 -600 -800
              , acceleration = vec2 0 200
              }
            ]

        model =
            { canvasWidth = 1200
            , canvasHeight = 400
            , kinematicObjects = listOfObs
            }
    in
    ( model, Cmd.none )


type Msg
    = TimeDelta Float


subscription : Model -> Sub Msg
subscription _ =
    E.onAnimationFrameDelta TimeDelta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeDelta delta ->
            ( { model
                | kinematicObjects =
                    List.map
                        (kinematicEngineCollide delta model.canvasHeight model.canvasWidth 25)
                    <|
                        model.kinematicObjects
              }
            , Cmd.none
            )


kinematicEngineCollide : Float -> Int -> Int -> Int -> KinematicObject -> KinematicObject
kinematicEngineCollide delta downLimit rightLimit size kinem =
    let
        collide =
            collision downLimit rightLimit size kinem
    in
    case collide of
        No ->
            kinematicEngine delta kinem

        _ ->
            kinematicEngine (3 * delta) <| reflect collide kinem


type Collide
    = Down
    | Up
    | Right
    | Left
    | No


collision : Int -> Int -> Int -> KinematicObject -> Collide
collision downLimit rightLimit size kinem =
    if (round <| getY kinem.position) < size then
        Up

    else if (round <| getY kinem.position) + size > downLimit then
        Down

    else if (round <| getX kinem.position) < size then
        Left

    else if (round <| getX kinem.position) + size > rightLimit then
        Right

    else
        No



reflect : Collide -> KinematicObject -> KinematicObject
reflect coll kinem =
    case coll of
        Up ->
            { kinem | velocity = scale 0.75 <| vec2 (getX kinem.velocity) <| dot (vec2 0 -1) kinem.velocity }

        Down ->
            { kinem | velocity = scale 0.75 <| vec2 (getX kinem.velocity) <| dot (vec2 0 -1) kinem.velocity }

        Left ->
            { kinem | velocity = scale 0.75 <| vec2 (dot (vec2 -1 0) kinem.velocity) <| getY kinem.velocity }

        Right ->
            { kinem | velocity = scale 0.75 <| vec2 (dot (vec2 -1 0) kinem.velocity) <| getY kinem.velocity }

        No ->
            kinem


kinematicEngine : Float -> KinematicObject -> KinematicObject
kinematicEngine delta kinem =
    let
        integration a b dt =
            add a <| scale (dt / 1000) b
    in
    { kinem
        | position = integration kinem.position kinem.velocity delta
        , velocity = integration kinem.velocity kinem.acceleration delta
    }


view : Model -> H.Html Msg
view model =
    div [ HA.class "content" ]
        [ h1 [] [ H.text "Hello Svg" ]
        , makeCanvas
            model.canvasWidth
            model.canvasHeight
          <|
            List.map makeObject model.kinematicObjects
        ]


makeObject : KinematicObject -> Object
makeObject kobj =
    { shape = Circle
    , pos =
        { x = round <| getX kobj.position
        , y = round <| getY kobj.position
        }
    , size = 25
    }
