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
import Isomorphism exposing (explanationOne, paneOne, animateIsomorphicTransition, isomorphicTransition, miniIsoGraph )
import MaxkCut exposing (MaxCutTransition, explanationTwo, paneTwo, animateMaxCutCompound, maxCutTransition, miniMaxGraph)
import GraphColoring exposing (ColorDisplaySeries, paneThree, explanationColoring, colorDisplaySeries, goColorSeries, miniColGraph)
import VertexCover exposing (VertexCoverDisplay, paneFour, explanationCover, vertexCoverDisplay, goCover)
import TreeWidth exposing (TreeWidthDisplay, paneTree, explanationWidth, treeWidthDisplay, goTree, miniTreeWidth)
import Graph exposing (ShapeTransition)
import FontSize exposing 
   ( getFontSize
   , FontSize(..)
   , DisplaySize
   , DeviceType(..)
   , getDeviceType
   )
import Buttons exposing(homeButton, aboutButton)

main : Program Flags Model Msg
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

type alias Flags =
   { width : Int
   , height : Int
   }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
   let
      displaySize =
         { width = flags.width
         , height = flags.height
         , deviceType = getDeviceType flags.width
         }
   in
   ({ helpStatus = False
    , url = url
    , key = key
    , displaySize = displaySize
    , topic = getTopic url
    }, Cmd.none)

type alias Model =
   { helpStatus : Bool
   , key : Nav.Key
   , url : Url.Url
   , displaySize : DisplaySize
   , topic : Topic
   }


type Topic =
   Isomorphic ShapeTransition
   | MaxCut MaxCutTransition
   | GraphColoring ColorDisplaySeries
   | VertexCover VertexCoverDisplay
   | TreeWidth TreeWidthDisplay
   | HomePage
   | About
   | ScreenSize

--type TopicName
--   = Iso
--   | Max
--   | Color
--   | Cover
--   | Tree
--
--type alias HomeState =
--   { topic : TopicName }


subscription : Model -> Sub Msg
subscription _ =
    Sub.batch
        [ E.onAnimationFrameDelta TimeDelta
        , E.onKeyPress keyDecoder
        , E.onResize (\w h -> GotNewScreen w h)
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
                     's' ->
                         GotoSize
                     'a' ->
                         GotoAbout
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
                GraphColoring displaySeries ->
                   GraphColoring ( goColorSeries displaySeries msg)
                VertexCover display ->
                   VertexCover ( goCover display msg)
                TreeWidth display ->
                   TreeWidth ( goTree display msg)
                HomePage ->
                   topic
                _ ->
                   topic

      helpStatus =
         case msg of
            ToggleHelpStatus ->
               not model.helpStatus
            TimeDelta _ ->
               model.helpStatus
            _ ->
               False

      oldDisplaySize =
         model.displaySize

      displaySize =
         case msg of
            GotNewScreen w h ->
              { oldDisplaySize 
                | width = w
                , height = h
                , deviceType = getDeviceType w
              }
            _ ->
               model.displaySize
               

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
             GotoSize ->
                   Nav.pushUrl model.key "/size"
             GotoAbout ->
                   Nav.pushUrl model.key "/about"


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
                   _ ->
                     Nav.pushUrl model.key "/"
                     

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
                     Nav.pushUrl model.key "/"
             _ ->
               Cmd.none
   in
   ({ model | 
         topic = newTopic
         , helpStatus = helpStatus
         , displaySize = displaySize
    }, command)

getTopic : Url.Url -> Topic
getTopic url =
   case (url.path) of
      "/isomorphism" ->
         Isomorphic isomorphicTransition
      "/maxkcut" ->
         MaxCut maxCutTransition
      "/coloring" ->
         GraphColoring colorDisplaySeries
      "/vertexcover" ->
         VertexCover vertexCoverDisplay
      "/treewidth" ->
         TreeWidth treeWidthDisplay
      "/" ->
         HomePage
      "/size" ->
         ScreenSize
      "/about" ->
         About
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
layOutAttributes deviceType = 
                  -- [ ELE.width ELE.fill
                   [ ELE.height ELE.fill
                   , Background.color <| ELE.rgb 0.2 0.2 0.2
                   --, ELE.padding 30
                   --, Font.size 18
                   , Font.size (getFontSize Normal deviceType)
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

viewbody : Model -> H.Html Msg
viewbody model =
   let
      heightOfHeader =
         round (toFloat model.displaySize.height * 0.085)
      heightOfRest =
         model.displaySize.height - heightOfHeader
      displaySize =
         DisplaySize model.displaySize.width heightOfRest model.displaySize.deviceType
   in
   ELE.layoutWith
      layOutOptions
      (layOutAttributes model.displaySize.deviceType)
      ( ELE.column
         [ ELE.width ELE.fill
         , ELE.height ELE.fill
         ]
         [ headerOfPage heightOfHeader
         , viewTopic model displaySize
         ]
      )

headerOfPage height =
   let
      buttonHeight =
         round (toFloat height * 0.50)
      padding =
         round <| toFloat (height - buttonHeight) * 0.25

   in 
   ELE.row
      [ ELE.width ELE.fill
      , ELE.height (ELE.fill |> ELE.maximum height)
      , Background.color (ELE.rgb 0.2 0.2 0.2)
      --, ELE.explain Debug.todo
      ]
      [ ELE.el [ELE.padding padding] (homeButton buttonHeight)
      , ELE.el [ELE.padding padding] (aboutButton buttonHeight)
      ]

showTopic displaySize =
   ELE.row
      [ ELE.width ELE.fill
      , ELE.height (ELE.fill |> ELE.maximum displaySize.height)
      ]


viewTopic model displaySize =
   let
      explanationSize =
         {displaySize | width = displaySize.width//2}
   in
   case model.topic of
      Isomorphic shapeTransition ->
            showTopic displaySize
                  [ displayColumn (paneOne shapeTransition.graphA shapeTransition.graphB)
                  , explanationOne shapeTransition model.helpStatus explanationSize
                  ]

      MaxCut maxCutTrans ->
            showTopic displaySize
                  [ displayColumn (paneTwo maxCutTrans) 
                  , explanationTwo maxCutTrans model.helpStatus explanationSize
                  ]

      GraphColoring display ->
            showTopic displaySize
                  [ displayColumn (paneThree display) 
                  , explanationColoring display model.helpStatus explanationSize
                  ]

      VertexCover display ->
            showTopic displaySize
                  [ displayColumn (paneFour display) 
                  , explanationCover display model.helpStatus explanationSize
                  ]

      TreeWidth display ->
            showTopic displaySize
                  [ displayColumn (paneTree display) 
                  , explanationWidth display model.helpStatus explanationSize
                  ]
      HomePage ->
            homePage displaySize.height

      ScreenSize ->
            ELE.el
                  [ ELE.width ELE.fill]
                  ( ELE.text <| String.fromInt model.displaySize.width
                               ++ " x " ++ String.fromInt model.displaySize.height
                  )

      About ->
            aboutPage (model.displaySize.width) (model.displaySize.height) 

aboutPage : Int -> Int -> ELE.Element Msg
aboutPage widthIn heightIn =
   let
      width = widthIn//3
      height = heightIn//4

      fatmasDetails = 
         ELE.row
            [ Border.rounded 40
            , Border.color (ELE.rgb 1 1 1)
            ]
            [ introFatma width (height)
            , photoGraph (widthIn - width) height
            ]
     
      superVisor =
         ELE.row
            [ Border.rounded 40
            , Border.color (ELE.rgb 1 1 1)
            ]
            [ introSuperVisor width height
            , photoGraphSuperVisor (widthIn - width) height
            ]
  in
  ELE.column
   []
   [ fatmasDetails
   , superVisor
   ]


photoGraph width height =
      ELE.column
         [ Font.color <| ELE.rgb 1 1 1
         , Font.heavy
         , ELE.spacingXY 10 15
         --, ELE.paddingXY 30 50
         , ELE.width (ELE.fill |> ELE.maximum (width - 1))
         , ELE.height (ELE.fill |> ELE.minimum (height))
         , Border.rounded 20
         , ELE.alignRight
         , Border.width 5
         , Border.color (ELE.rgb 0.7 0.6 0.6)
         ]
         [ ELE.el 
               [ Border.rounded 200
               ]
               (ELE.image [ ELE.width (ELE.fill |> ELE.maximum 400)
                          ] 
                     { src = "images/fatma.jpeg" 
                     , description = ""
                     }
               )
         ]

photoGraphSuperVisor width height =
      ELE.column
         [ Font.color <| ELE.rgb 1 1 1
         , Font.heavy
         , ELE.spacingXY 10 15
         --, ELE.paddingXY 30 50
         , ELE.width (ELE.fill |> ELE.maximum (width - 1))
         , ELE.height (ELE.fill |> ELE.minimum (height))
         , Border.rounded 20
         , ELE.alignRight
         , Border.width 5
         , Border.color (ELE.rgb 0.7 0.6 0.6)
         ]
         [ ELE.el 
               [ Border.rounded 200
               ]
               (ELE.image [ ELE.width (ELE.fill |> ELE.maximum 400)
                          ] 
                     { src = "images/sofiat.jpg" 
                     , description = ""
                     }
               )
         ]

introFatma width height =
      let
         fatmasIntro =
            """
            I am a third year Software Engineering student in University
            of Glasgow. This web app was build for the as my final year
            project. My intrests are maths and functional programming.
            """
            
      in
      ELE.column
         [ Font.color <| ELE.rgb 1 1 1
         , Font.heavy
         , ELE.spacingXY 10 15
         , ELE.paddingXY 30 50
         , ELE.width (ELE.fill |> ELE.maximum (width))
         , ELE.height (ELE.fill |> ELE.minimum (height))
         , Border.rounded 10
         , ELE.alignLeft
         ]
         <| [ ELE.el [ Font.size 30]
                  (ELE.text "Fatma Alsayegh")
            , ELE.paragraph [
                            ] 
                           [ ELE.el 
                                     [
                                     ]  
                                     (ELE.text "Hi I am ")
                           , ELE.el 
                                     [ Font.color <| ELE.rgb 0.5 0.9 0.7
                                     , Font.size 25
                                     ]
                                     (ELE.text "Fatma! ")
                           , ELE.el 
                                     []
                                     (ELE.text fatmasIntro)
                           ]
            ]

introSuperVisor width height =
      let
         intro =
            """
             Olaosebikan is a Lecturer in Algorithms and Complexity in the
            School of Computing Science at the University of Glasgow. She has a
            PhD in Computing Science from the University of Glasgow, Scotland;
            an MSc in Mathematical Sciences from the African Institute for
            Mathematical Sciences (AIMS), Ghana; and a BSc in Mathematics from
            the University of Ibadan, Nigeria.  In her spare time she finds
            myself reflecting on how best to inspire and pass on computing
            skills to young Africans in STEM, with the hope that they can also
            grow to become creative thinkers.  """
      in
      ELE.column
         [ Font.color <| ELE.rgb 1 1 1
         , Font.heavy
         , ELE.spacingXY 10 15
         , ELE.paddingXY 30 50
         , ELE.width (ELE.fill |> ELE.maximum (width))
         , ELE.height (ELE.fill |> ELE.minimum (height))
         , Border.rounded 10
         , ELE.alignRight
         ]
         <| [ ELE.el [ Font.size 30]
                  (ELE.text "My Supervisor.")
            , ELE.paragraph [
                            ] 
                           [ ELE.el 
                                     [ Font.color <| ELE.rgb 0.5 0.9 0.7
                                     , Font.size 25
                                     ]
                                     (ELE.text "Sofiat ")
                           , ELE.el 
                                     []
                                     (ELE.text intro)
                           ]
            ]


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
               
      miniGraph =
         case topicMsg of
            GotoIsomorphism ->
               miniIsoGraph
            GotoMaxkCut ->
               miniMaxGraph
            GotoColoring ->
               miniColGraph
            GotoCover ->
               miniIsoGraph
            GotoTreeWidth ->
               miniTreeWidth
            _ ->
               miniIsoGraph
   in
   ELE.column [ Events.onClick topicMsg
              , ELE.pointer
              , ELE.paddingXY 13 15
              , Border.solid
              , Border.width 2
              , Border.rounded 15
              , ELE.width (ELE.fill |> ELE.minimum 200)
              ] 
              [ displayMiniGraph miniGraph
              , ELE.el [ELE.centerX] <| ELE.text tex
              ]

displayMiniGraph svgHtml =
   ELE.el
      [ Font.color (ELE.rgb 1 1 1)
      , ELE.height ELE.fill
      , ELE.width ELE.fill
      , Background.color <| ELE.rgb 0.2 0.2 0.2
      ] (ELE.html svgHtml)

homePage : Int -> ELE.Element Msg
homePage height =
   ELE.column
      [ ELE.centerX
      , ELE.centerY
      , Font.color <| ELE.rgb 1 1 1
      , Font.heavy
      , ELE.spacingXY 10 15
      ]
      <|[ ELE.paragraph [ Font.size 45
                        , ELE.paddingXY 5 20
                        ] 
                        [ ELE.el 
                            [ Font.color <| ELE.rgb 0.5 0.9 0.7
                            , Font.size 70
                            ]
                            (ELE.text "V")
                        , ELE.text "isualization of "
                        , ELE.el
                            [ Font.color <| ELE.rgb 0.5 0.9 0.7
                            , Font.size 70
                            ]
                            (ELE.text "C")
                        , ELE.text "lassical "
                        , ELE.el
                            [ Font.color <| ELE.rgb 0.5 0.9 0.7
                            , Font.size 70
                            ]
                           (ELE.text "G")
                        , ELE.text "raph "
                        , ELE.el
                            [ Font.color <| ELE.rgb 0.5 0.9 0.7
                            , Font.size 70
                            ]
                            (ELE.text "T")
                        , ELE.text "heory "
                        , ELE.el
                            [ Font.color <| ELE.rgb 0.5 0.9 0.7
                            , Font.size 70
                            ]
                            (ELE.text"P")
                        , ELE.text "roblems"
                        ]
        , ELE.row
               [ ELE.centerX
               , ELE.centerY
               , Font.color <| ELE.rgb 1 1 1
               , Font.heavy
               , ELE.spacingXY 10 15
               ]
               <| List.map makeTopicIcon 
                  [ GotoIsomorphism
                  , GotoMaxkCut
                  , GotoColoring
                  ]
        , ELE.row
               [ ELE.centerX
               , ELE.centerY
               , Font.color <| ELE.rgb 1 1 1
               , Font.heavy
               , ELE.spacingXY 10 15
               ]
               <| List.map makeTopicIcon 
                  [ GotoCover
                  , GotoTreeWidth
                  ]
         ]
