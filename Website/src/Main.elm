module Main exposing (..)

import Browser
import Browser.Events as E
import Color exposing (Color)
import Explanation exposing (..)
import Html as H exposing (div, h1, p, text)
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (..)
import Json.Decode as Decode
import Math.Matrix4 as M4 exposing (..)
import Math.Vector3 exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)
import String.Format
import Point2d as Pt
import LineSegment2d as Ln
import Length as Len
import Element as ELE
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Element.Input as Input
import Ant.Icon as Ant
import Ant.Icons as Icons
import Messages exposing (Msg(..))
import Isomorphism exposing (explanationOne, paneOne, animateIsomorphicTransition, isomorphicTransition)
import MaxkCut exposing (MaxCutTransition, explanationTwo, paneTwo, animateMaxCutCompound, maxCutTransition)
import GraphColoring exposing (ColorDisplay, paneThree, explanationColoring, colorDisplay, goColor)
import VertexCover exposing (VertexCoverDisplay, paneFour, explanationCover, vertexCoverDisplay, goCover)
import TreeWidth exposing (TreeWidthDisplay, paneTree, explanationWidth, treeWidthDisplay, goTree)
import Graph exposing (ShapeTransition)

main : Program () SuperModel Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscription
        }

init : () -> ( SuperModel, Cmd Msg )
init _ =
    let
        shapeTransition =
            isomorphicTransition
        model = ( Isomorphic shapeTransition)
    in
    ({ helpStatus = False
    , model = model
    }, Cmd.none)

type alias SuperModel =
   { helpStatus : Bool
   , model : Model
   }

type Model =
   Isomorphic ShapeTransition
   | MaxCut MaxCutTransition
   | GraphColoring ColorDisplay
   | VertexCover VertexCoverDisplay
   | TreeWidth TreeWidthDisplay




subscription : SuperModel -> Sub Msg
subscription _ =
    Sub.batch
        [ E.onAnimationFrameDelta TimeDelta
        , E.onKeyPress keyDecoder
        ]

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map keyToMsg (Decode.field "key" Decode.string)


keyToMsg : String -> Msg
keyToMsg value =
    case String.uncons value of
        Just ( char, "" ) ->
            case (Char.isDigit char) of
               True ->
                  String.cons char "" |> String.toInt |> chooseVertexFromInt
               False ->
                  case char of
                     'r' ->
                         AnimationStartOver

                     'p' ->
                         AnimationToggle

                     'n' ->
                         NextTopic

                     'N' ->
                         PreviousTopic

                     'l' ->
                         MaxCutLine 
                     'w' ->
                         VertexNonColor
                     't' ->
                         NextAnimation
                     'T' ->
                         PreviousTreeWidthAnimation
                     'h' ->
                         ToggleHelpStatus
                     _ ->
                         Other
        _ ->
            Other

chooseVertexFromInt : Maybe Int -> Msg
chooseVertexFromInt x =
   case x of
      Nothing
         -> Other
      Just name
         -> ToggleVertexStatus name



update : Msg -> SuperModel -> ( SuperModel, Cmd Msg )
update msg superModel =
    let
      model = superModel.model
      newModel =
         case msg of
           NextTopic ->
              case model of
                 Isomorphic x ->
                    ( MaxCut maxCutTransition)
                 MaxCut x ->
                    (GraphColoring colorDisplay)
                 GraphColoring x ->
                    ( VertexCover vertexCoverDisplay)
                 VertexCover x ->
                    ( TreeWidth treeWidthDisplay)
                 TreeWidth x ->
                    ( Isomorphic isomorphicTransition)

           PreviousTopic ->
              case model of
                 Isomorphic x ->
                    ( TreeWidth treeWidthDisplay)
                 TreeWidth x ->
                    ( VertexCover vertexCoverDisplay)
                 MaxCut x ->
                    ( Isomorphic isomorphicTransition)
                 GraphColoring x ->
                    ( MaxCut maxCutTransition)
                 VertexCover x ->
                    (GraphColoring colorDisplay)
           _ ->
              case model of
                Isomorphic shapeTransition ->
                   ( Isomorphic (animateIsomorphicTransition msg shapeTransition))
                MaxCut maxcutTrans ->
                   ( MaxCut (animateMaxCutCompound msg maxcutTrans))
                GraphColoring display ->
                   GraphColoring ( goColor display msg)
                VertexCover display ->
                   VertexCover ( goCover display msg)
                TreeWidth display ->
                   TreeWidth ( goTree display msg)
                --_ ->
                --  model

      helpStatus =
         case msg of
            ToggleHelpStatus ->
               not superModel.helpStatus
            TimeDelta _ ->
               superModel.helpStatus
            _ ->
               False
            
   in
   ({ superModel | model = newModel, helpStatus = helpStatus }, Cmd.none)

layOutOptions =
   { options =
      [ ELE.focusStyle
         { borderColor = Nothing
         , backgroundColor = Nothing
         , shadow = Nothing
         }
       ]
   } 

--layOutAttributes = [ELE.width ELE.fill, ELE.height ELE.fill]
layOutAttributes = 
                  -- [ ELE.width ELE.fill
                   [ ELE.height ELE.fill
                   , Background.color <| ELE.rgb 0.2 0.2 0.2
                   , ELE.padding 30
                   , Font.size 18
                   ]

displayColumn svgHtml =
   ELE.column
      [ Font.color (ELE.rgb 1 1 1)
      , ELE.height ELE.fill
      , ELE.width ELE.fill
      , Background.color <| ELE.rgb 0.2 0.2 0.2
      ] [ELE.html svgHtml]

view superModel =
   case superModel.model of
      Isomorphic shapeTransition ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ELE.width ELE.fill
                  ]

                  [ displayColumn (paneOne shapeTransition.graphA shapeTransition.graphB)
                  , explanationOne shapeTransition superModel.helpStatus
                  ]
            )

      MaxCut maxCutTrans ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneTwo maxCutTrans) 
                  , explanationTwo maxCutTrans superModel.helpStatus
                  ]
            )

      GraphColoring display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneThree display) 
                  , explanationColoring display superModel.helpStatus
                  ]
            )

      VertexCover display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneFour display) 
                  , explanationCover display superModel.helpStatus
                  ]
            )

      TreeWidth display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneTree display) 
                  , explanationWidth display superModel.helpStatus
                  ]
            )
