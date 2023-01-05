module Main exposing (..)

import Browser
import Browser.Events as E
import Url
import Url.Parser as Parser
import Browser.Navigation as Nav
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
import Element.Events as Events
import Ant.Icon as Ant
import Ant.Icons as Icons
import Messages exposing (Msg(..))
import Isomorphism exposing (explanationOne, paneOne, animateIsomorphicTransition, isomorphicTransition)
import MaxkCut exposing (MaxCutTransition, explanationTwo, paneTwo, animateMaxCutCompound, maxCutTransition)
import GraphColoring exposing (ColorDisplay, paneThree, explanationColoring, colorDisplay, goColor)
import VertexCover exposing (VertexCoverDisplay, paneFour, explanationCover, vertexCoverDisplay, goCover)
import TreeWidth exposing (TreeWidthDisplay, paneTree, explanationWidth, treeWidthDisplay, goTree)
import Graph exposing (ShapeTransition)

main : Program () Model Msg
main =
    --Browser.element
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscription
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
   ({ helpStatus = False
    , url = url
    , key = key
    , topic = getTopic url
    }, Cmd.none)

type alias Model =
   { helpStatus : Bool
   , key : Nav.Key
   , url : Url.Url
   , topic : Topic
   }

type Topic =
   Isomorphic ShapeTransition
   | MaxCut MaxCutTransition
   | GraphColoring ColorDisplay
   | VertexCover VertexCoverDisplay
   | TreeWidth TreeWidthDisplay
   | HomePage




subscription : Model -> Sub Msg
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
                     'c' ->
                         GotoHome
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



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
      topic = model.topic
      newTopic =
         case msg of
           UrlChanged url ->
                 getTopic url

           _ ->
              case topic of
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
                HomePage ->
                   topic

      helpStatus =
         case msg of
            ToggleHelpStatus ->
               not model.helpStatus
            TimeDelta _ ->
               model.helpStatus
            _ ->
               False

      command =
          case msg of
             LinkClicked urlRequest ->
               case urlRequest of
                  Browser.Internal url ->
                     Nav.pushUrl model.key (Url.toString url)
                  Browser.External href ->
                     Nav.load href
             GotoHome ->
                   Nav.pushUrl model.key "/"
             GotoIsomorphism ->
                   Nav.pushUrl model.key "/isomorphism"
             GotoMaxkCut ->
                   Nav.pushUrl model.key "/maxkcut"
             GotoColoring ->
                   Nav.pushUrl model.key "/coloring"
             GotoCover ->
                   Nav.pushUrl model.key "/vertexcover"
             GotoTreeWidth ->
                   Nav.pushUrl model.key "/treewidth"

             NextTopic ->
                case topic of
                   HomePage ->
                     Nav.pushUrl model.key "/isomorphism"
                   Isomorphic x ->
                     Nav.pushUrl model.key "/maxkcut"
                   MaxCut x ->
                     Nav.pushUrl model.key "/coloring"
                   GraphColoring x ->
                     Nav.pushUrl model.key "/vertexcover"
                   VertexCover x ->
                     Nav.pushUrl model.key "/treewidth"
                   TreeWidth x ->
                     Nav.pushUrl model.key "/isomorphism"

             PreviousTopic ->
                case topic of
                   Isomorphic x ->
                     Nav.pushUrl model.key "/treewidth"
                   TreeWidth x ->
                     Nav.pushUrl model.key "/vertexcover"
                   MaxCut x ->
                     Nav.pushUrl model.key "/isomorphism"
                   GraphColoring x ->
                     Nav.pushUrl model.key "/maxkcut"
                   VertexCover x ->
                     Nav.pushUrl model.key "/coloring"
                   HomePage ->
                     Cmd.none
             _ ->
               Cmd.none
   in
   ({ model | topic = newTopic, helpStatus = helpStatus}, command)

getTopic : Url.Url -> Topic
getTopic url =
   case (url.path) of
      "/isomorphism" ->
         Isomorphic isomorphicTransition
      "/maxkcut" ->
         MaxCut maxCutTransition
      "/coloring" ->
         GraphColoring colorDisplay
      "/vertexcover" ->
         VertexCover vertexCoverDisplay
      "/treewidth" ->
         TreeWidth treeWidthDisplay
      "/" ->
         HomePage
      _ ->
         HomePage


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

view : Model -> Browser.Document Msg
view model =
   { title = "Visualization"
   , body  = [ viewbody model ]
   }

viewbody model =
   case model.topic of
      Isomorphic shapeTransition ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ELE.width ELE.fill
                  ]

                  [ displayColumn (paneOne shapeTransition.graphA shapeTransition.graphB)
                  , explanationOne shapeTransition model.helpStatus
                  ]
            )

      MaxCut maxCutTrans ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneTwo maxCutTrans) 
                  , explanationTwo maxCutTrans model.helpStatus
                  ]
            )

      GraphColoring display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneThree display) 
                  , explanationColoring display model.helpStatus
                  ]
            )

      VertexCover display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneFour display) 
                  , explanationCover display model.helpStatus
                  ]
            )

      TreeWidth display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneTree display) 
                  , explanationWidth display model.helpStatus
                  ]
            )
      HomePage ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.column
               [ ELE.centerX
               , ELE.centerY
               , Font.color <| ELE.rgb 1 1 1
               , Font.heavy
               ]
               <|[ ELE.el [ Font.size 50
                          , ELE.paddingXY 5 20
                          ] 
                          (ELE.text "Visualization of Classical Graph Theory Problems")
                 ]
                 ++  List.map makeTopicIcon 
                        [ GotoIsomorphism
                        , GotoMaxkCut
                        , GotoColoring
                        , GotoCover
                        , GotoTreeWidth
                        ]
            )


makeTopicIcon : Msg -> ELE.Element Msg
makeTopicIcon topicMsg =  
   let
      tex =
         case topicMsg of
            GotoIsomorphism ->
               "Graph Isomorphism."
            GotoMaxkCut ->
               "Max k Cut."
            GotoColoring ->
               "Graph Coloring."
            GotoCover ->
               "Vertex Cover."
            GotoTreeWidth ->
               "Tree Width."
            _ ->
               "Oops"
   in
   ELE.el [ Events.onClick topicMsg
          , ELE.pointer
          , ELE.padding 13
          ] 
          (ELE.text tex)

