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

main : Program () SuperModel Msg
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


init : () -> Url.Url -> Nav.Key -> ( SuperModel, Cmd Msg )
init flags url key =
   ({ helpStatus = False
    , url = url
    , key = key
    , model = getTopicModel url
    }, Cmd.none)

type alias SuperModel =
   { helpStatus : Bool
   , key : Nav.Key
   , url : Url.Url
   , model : Model
   }

type Model =
   Isomorphic ShapeTransition
   | MaxCut MaxCutTransition
   | GraphColoring ColorDisplay
   | VertexCover VertexCoverDisplay
   | TreeWidth TreeWidthDisplay
   | HomePage




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



update : Msg -> SuperModel -> ( SuperModel, Cmd Msg )
update msg superModel =
    let
      model = superModel.model
      newModel =
         case msg of
           UrlChanged url ->
                 getTopicModel url

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
                HomePage ->
                   model

      helpStatus =
         case msg of
            ToggleHelpStatus ->
               not superModel.helpStatus
            TimeDelta _ ->
               superModel.helpStatus
            _ ->
               False

      command =
          case msg of
             LinkClicked urlRequest ->
               case urlRequest of
                  Browser.Internal url ->
                     Nav.pushUrl superModel.key (Url.toString url)
                  Browser.External href ->
                     Nav.load href
             GotoHome ->
                   Nav.pushUrl superModel.key "/"
             GotoIsomorphism ->
                   Nav.pushUrl superModel.key "/isomorphism"
             GotoMaxkCut ->
                   Nav.pushUrl superModel.key "/maxkcut"
             GotoColoring ->
                   Nav.pushUrl superModel.key "/coloring"
             GotoCover ->
                   Nav.pushUrl superModel.key "/vertexcover"
             GotoTreeWidth ->
                   Nav.pushUrl superModel.key "/treewidth"

             NextTopic ->
                case model of
                   HomePage ->
                     Nav.pushUrl superModel.key "/isomorphism"
                   Isomorphic x ->
                     Nav.pushUrl superModel.key "/maxkcut"
                   MaxCut x ->
                     Nav.pushUrl superModel.key "/coloring"
                   GraphColoring x ->
                     Nav.pushUrl superModel.key "/vertexcover"
                   VertexCover x ->
                     Nav.pushUrl superModel.key "/treewidth"
                   TreeWidth x ->
                     Nav.pushUrl superModel.key "/isomorphism"

             PreviousTopic ->
                case model of
                   Isomorphic x ->
                     Nav.pushUrl superModel.key "/treewidth"
                   TreeWidth x ->
                     Nav.pushUrl superModel.key "/vertexcover"
                   MaxCut x ->
                     Nav.pushUrl superModel.key "/isomorphism"
                   GraphColoring x ->
                     Nav.pushUrl superModel.key "/maxkcut"
                   VertexCover x ->
                     Nav.pushUrl superModel.key "/coloring"
                   HomePage ->
                     Cmd.none
             _ ->
               Cmd.none
   in
   ({ superModel | model = newModel, helpStatus = helpStatus}, command)

getTopicModel : Url.Url -> Model
getTopicModel url =
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

view : SuperModel -> Browser.Document Msg
view supermodel =
   { title = "Visualization"
   , body  = [ viewbody supermodel ]
   }

viewbody superModel =
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

