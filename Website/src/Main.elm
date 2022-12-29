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
import Graph as GR





-- Main Program
-- main is the main program
-- init will initialize the model
-- view uses the model to populate the app
-- update updates the model
-- subscription subscribes to the clock


main : Program () SuperModel Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscription
        }



-- Model of the app
-- Model contains the data model of the app.
-- Model has currently has 2 graphs
-- and a grid, graphB will transform into slowly.

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





         



init : () -> ( SuperModel, Cmd Msg )
init _ =
    let
        initialGraph =
            makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)

        shapeTransition =
            { graphA = initialGraph
            , graphB = initialGraph
            , finalGrid = bipartiteGrid
            , animationOn = False
            , specialToken = NoToken
            }
        model = ( Isomorphic shapeTransition)
    in
    ({ helpStatus = False
    , model = model
    }, Cmd.none)





-- Subscribing to Animation frame clock.
-- Generates a Msg which can be used by update function


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



-- Update the model
-- Update function takes in messages from the webpage or the subscriber
-- and uses them to modify the model
-- 1. With every animation clock tick, it changes the graph to move towards
--    a specified grid.
-- 2. With MouseOver a vertex it makes that vertices' incident edges glow.
-- 3. With MouseOut from a vertex it makes that vertices' incident edges not glow.


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
                   ( goColor display msg)
                VertexCover display ->
                   ( goCover display msg)
                TreeWidth display ->
                   ( goTree display msg)
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







-- View the Model
-- This function is responsible for the actual rendering
-- of the webpage. Any change in the model by update function is reflected in the
-- webpage as view works with the latest model.

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
                  --, Background.color <| ELE.rgb 44 44 44
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







-- Move a graph towards grid points.
-- This function takes as inputs a graph, and a target grid
-- First an intermediate grid is created near the input graph, which has the placeholder for
-- the vertices a little bit towards the final destination.
-- then the original graph is morphed into the intermediate grid.


type alias Size =
    Int

-- put edges first and then vertices
-- and produces a single list


findTwoPositions : List Vertex -> ( (Int, Int, Int), (Int, Int, Int) ) -> Maybe (Vec3, Vec3)
findTwoPositions vs x =
   case x of
      ((a1, b1, c1), (a2, b2, c2)) ->
         let 
            pos1 = findCenterOfTriple a1 b1 c1 vs
            pos2 = findCenterOfTriple a2 b2 c2 vs
         in
         case (pos1, pos2) of
            (Nothing, _) ->
               Nothing
            (_, Nothing) ->
               Nothing
            (Just p1, Just p2) ->
               Just (p1, p2)
            
      

findCenterOfTriple : Int -> Int -> Int -> List Vertex -> Maybe Vec3
findCenterOfTriple a b c vs =
   case (lookUpVertex a vs, lookUpVertex b vs, lookUpVertex c vs) of
      (Nothing, _, _) ->
         Nothing
      (_, Nothing, _) ->
         Nothing
      (_, _, Nothing) ->
         Nothing
      (Just v1 , Just v2, Just v3) ->
            Just <| 
            ( Math.Vector3.add v1.pos v2.pos
            |> Math.Vector3.add v3.pos
            |> Math.Vector3.scale 0.333
            )

      
            







