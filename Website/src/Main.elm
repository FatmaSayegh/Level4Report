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





-- Main Program
-- main is the main program
-- init will initialize the model
-- view uses the model to populate the app
-- update updates the model
-- subscription subscribes to the clock


main : Program () Model Msg
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


type Model =
   Isomorphic ShapeTransition
   | MaxCut MaxCutTransition
   | GraphColoring ColorDisplay
   | VertexCover VertexCoverDisplay
   | TreeWidth TreeWidthDisplay

type alias TreeWidthDisplay =
   { graph : Graph
   , gridCircular : Grid
   , gridHoneyComb : Grid
   , triples : List (Int, Int, Int)
   , treeLines : List ( (Int, Int, Int), (Int, Int, Int) )
   , status : TreeWidthStatus
   }

type TreeWidthStatus =
   CircularGraph
   | MorphingIntoHoneyComb
   | HoneyCombGraph
   | ShowOnePiece
   | PiecesMarked
   | TreeDrawnGraph

type alias ColorDisplay =
   { graphA : Graph
   , chosenColor : Color
   , defaultColor : Color
   }

type alias VertexCoverDisplay =
   { graphA : Graph
   }

type Token =
   MakeKCut
   | NoToken

type alias ShapeTransition =
    { graphA : Graph -- Will remain static
    , graphB : Graph  -- Will move towards final Grid when animationOn is True
    , finalGrid : Grid 
    , animationOn : Bool
    , specialToken : Token
    }

type alias MaxCutTransition =
    { transitionA : ShapeTransition -- Will remain static
    , transitionB : ShapeTransition  -- Will move towards final Grid when animationOn is True
    , state : MaxCutState
    }

type MaxCutState =
   TwoCut
   | ThreeCut

-- Initializing the model
-- init function initializes, a model and provides
-- an instance of the model to the elm runtime.


isomorphicTransition : ShapeTransition
isomorphicTransition =
    let
        initialGraph =
            makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)
    in
        { graphA = initialGraph
        , graphB = initialGraph
        , finalGrid = bipartiteGrid
        , animationOn = False
        , specialToken = NoToken
        }

maxcutTransitionA : ShapeTransition
maxcutTransitionA =
    let
        (initialGraph, finalGrid) =
            maxCutGeometry
    in
        { graphA = initialGraph
        , graphB = initialGraph
        , finalGrid = finalGrid
        , animationOn = False
        , specialToken = NoToken
        }

maxcutTransitionB : ShapeTransition
maxcutTransitionB =
    let
        (initialGraph, finalGrid) =
            threeCutGeometry
    in
        { graphA = initialGraph
        , graphB = initialGraph
        , finalGrid = finalGrid
        , animationOn = False
        , specialToken = NoToken
        }

maxCutTransition : MaxCutTransition
maxCutTransition =
   { transitionA = maxcutTransitionA
   , transitionB = maxcutTransitionB
   , state = TwoCut
   }

maxCutGeometry : (Graph, Grid)
maxCutGeometry =
   let
      position = (vec3 100 200 0) -- left center
      verticalShift = (vec3 0 50 0)
      verticalShiftGrid = (vec3 0 90 0)
      horizontalShiftGrid = (vec3 200 0 0)
      setA = [1,2,3,4]
      setB = [5,6,7,8]
      --edgeTuples = [ (1, 8), (1, 7), (1, 6), (2, 5), (2, 6), (2, 7), (3, 7), (3, 8), (3, 6), (3, 4), (4, 5)]
      edgeTuples = [ (1, 8), (1, 7), (2, 6), (2, 7), (3, 5), (3, 7), (3, 8), (3, 6), (3, 4), (4, 5)]
      setAGrid = parametricPolygon 4 (vec3 50 30 0) (sub position  verticalShift) (pi/3)
      setBGrid = parametricPolygon 4 (vec3 50 30 0) (add position verticalShift) (pi/6)
      setAGridPosition = (add (sub (sub position verticalShift) verticalShiftGrid) horizontalShiftGrid)
      setBGridPosition = (add (add (add position verticalShift) verticalShiftGrid) horizontalShiftGrid)
      setAFinalGrid = parametricPolygon 4 (vec3 70 10 0) setAGridPosition (pi/3)
      setBFinalGrid = parametricPolygon 4 (vec3 70 10 0) setBGridPosition (pi/3)
      vertices = 
         List.map3 (\name g c -> Vertex name g c False) 
            (setA ++ setB)
            (setAGrid ++ setBGrid)
            (listOfColors First 8)
      edges =
         makeEdgesWithTuples edgeTuples vertices

      in
      (Graph vertices edges, setAFinalGrid ++ setBFinalGrid)

threeCutGeometry : (Graph, Grid)
threeCutGeometry =
   let
      position = (vec3 200 180 0) -- left center
      distance =
         100
      angle = pi/6
      verticalShift = vec3 0 (distance * sin angle) 0
      longVerticalShift = vec3 0 distance 0
      horizontalShift = vec3 (distance * cos angle) 0 0
      edgeTuples 
         = [  (1, 4), (1, 5), (1, 6), (1, 7), (1, 8), (1, 9)
           ,  (2, 4), (2, 5), (2, 6), (2, 7), (2, 8), (2, 9)
           ,  (3, 4), (3, 5), (3, 6), (3, 7), (3, 8), (3, 9)
           ,  (4, 7), (4, 8), (4, 9)
           ,  (5, 7), (5, 8), (5, 9)
           ,  (6, 7), (6, 8), (6, 9)
           ]
      gridStart = parametricPolygon 9 (vec3 80 80 0) position (pi/2 - 2*pi/9)
      modifiedGrid =
         gridStart
            |> List.map2 (\x y -> (x, y)) (List.range 1 9)
            |> List.map (\t ->
                           if Tuple.first t <= 3
                              then
                                 (add (Tuple.second t) longVerticalShift)
                              else
                                 if Tuple.first t > 3 && Tuple.first t <= 6
                                    then
                                    (sub (sub (Tuple.second t) horizontalShift) verticalShift)
                                    else
                                    (sub (add (Tuple.second t) horizontalShift) verticalShift)
                        )

      vertices = 
         List.map3 (\name g c -> Vertex name g c False) 
            (List.range 1 9)
            (gridStart)
            (listOfColors First 9)
      edges =
         makeEdgesWithTuples edgeTuples vertices

      in
      (Graph vertices edges, modifiedGrid)
         


makeEdgesWithTuples : List (Int, Int) -> List Vertex -> List Edge
makeEdgesWithTuples tuples vertices =
   case tuples of
      [] ->
         []
      (tu::tus) ->
         case (makeEdgeWithTuple tu vertices) of 
            Nothing ->
               makeEdgesWithTuples tus vertices
            (Just edge) ->
               edge :: makeEdgesWithTuples tus vertices



makeEdgeWithTuple : (Int, Int) -> List Vertex -> Maybe Edge
makeEdgeWithTuple tu vs =
   case tu of
      (name1, name2) ->
         case (lookUpVertex name1 vs, lookUpVertex name2 vs) of
            (Nothing, _ ) ->
               Nothing
            ( _, Nothing ) ->
               Nothing
            (Just vertexOne, Just vertexTwo) ->
               Just (Edge vertexOne vertexTwo)


init : () -> ( Model, Cmd Msg )
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
    in
    ( Isomorphic shapeTransition, Cmd.none )



-- Msg
-- This data type contains the kinds of messages
-- the html page or the or a subscriber (animation clock in this app) may give to the elm-runtime.


type Msg
    = TimeDelta Float
    | HoverOver Int
    | MouseOut Int
    | VertexClicked Int
    | AnimationToggle
    | AnimationStartOver
    | ToggleVertexStatus Int
    | NextTopic
    | PreviousTopic
    | MaxCutLine
    | ColoringSelectColor Color
    | VertexNonColor
    --| NextTreeWidthAnimation 
    | NextAnimation 
    | PreviousTreeWidthAnimation
    | Other


-- Subscribing to Animation frame clock.
-- Generates a Msg which can be used by update function


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
                         --NextTreeWidthAnimation
                         NextAnimation
                     'T' ->
                         PreviousTreeWidthAnimation
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      NextTopic ->
         case model of
            Isomorphic x ->
               ( MaxCut maxCutTransition, Cmd.none)
            MaxCut x ->
               (GraphColoring colorDisplay, Cmd.none)
            GraphColoring x ->
               ( VertexCover vertexCoverDisplay, Cmd.none)
            VertexCover x ->
               ( TreeWidth treeWidthDisplay, Cmd.none)
            TreeWidth x ->
               ( Isomorphic isomorphicTransition, Cmd.none)

      PreviousTopic ->
         case model of
            Isomorphic x ->
               ( TreeWidth treeWidthDisplay, Cmd.none)
            TreeWidth x ->
               ( VertexCover vertexCoverDisplay, Cmd.none)
            MaxCut x ->
               ( Isomorphic isomorphicTransition, Cmd.none)
            GraphColoring x ->
               ( MaxCut maxCutTransition, Cmd.none)
            VertexCover x ->
               (GraphColoring colorDisplay, Cmd.none)
      _ ->
         case model of
           Isomorphic shapeTransition ->
              ( Isomorphic (animateIsomorphicTransition msg shapeTransition), Cmd.none )
           MaxCut maxcutTrans ->
              ( MaxCut (animateMaxCutCompound msg maxcutTrans), Cmd.none )
           GraphColoring display ->
              ( goColor display msg, Cmd.none)
           VertexCover display ->
              ( goCover display msg, Cmd.none)
           TreeWidth display ->
              ( goTree display msg, Cmd.none)

goTree : TreeWidthDisplay -> Msg -> Model
goTree display msg =
   case msg of

      --NextTreeWidthAnimation ->
      NextAnimation ->
         let 
           newStatus =
               case display.status of
                  CircularGraph ->
                     MorphingIntoHoneyComb
                  MorphingIntoHoneyComb ->
                     HoneyCombGraph
                  HoneyCombGraph ->
                     ShowOnePiece
                  ShowOnePiece ->
                     PiecesMarked 
                  PiecesMarked ->
                     TreeDrawnGraph
                  TreeDrawnGraph ->
                     CircularGraph

           newGraph =
               case newStatus of
                   CircularGraph ->
                     morphGraph display.graph display.gridCircular
                   HoneyCombGraph ->
                     morphGraph display.graph display.gridHoneyComb
                   _ ->
                     display.graph

           newDisplay =
               { display
                 | status = newStatus
                 , graph = newGraph
               }

          in
          TreeWidth newDisplay

      PreviousTreeWidthAnimation ->
         let 
           newStatus =
               case display.status of
                  CircularGraph ->
                     CircularGraph
                  MorphingIntoHoneyComb ->
                     CircularGraph
                  HoneyCombGraph ->
                     CircularGraph
                  ShowOnePiece ->
                     HoneyCombGraph
                  PiecesMarked ->
                     ShowOnePiece
                  TreeDrawnGraph ->
                     PiecesMarked

           newGraph =
               case newStatus of
                   CircularGraph ->
                     morphGraph display.graph display.gridCircular
                   HoneyCombGraph ->
                     morphGraph display.graph display.gridHoneyComb
                   _ ->
                     display.graph

           newDisplay =
               { display
                 | status = newStatus
                 , graph = newGraph
               }

          in
          TreeWidth newDisplay

      TimeDelta delta ->
         if display.status == MorphingIntoHoneyComb
            then
              TreeWidth <| morphIntoHoneyComb display
            else
              TreeWidth display
      
      _ ->
         TreeWidth display


morphIntoHoneyComb : TreeWidthDisplay -> TreeWidthDisplay
morphIntoHoneyComb display =
   if (distanceBetweenGraphAndGrid display.graph display.gridHoneyComb < 20)
      then { display
               | status = HoneyCombGraph
               , graph = morphGraph display.graph display.gridHoneyComb
           }
      else
           { display
               | graph = moveTowards display.graph display.gridHoneyComb
           }

goCover : VertexCoverDisplay -> Msg -> Model
goCover display msg =
   case msg of
       ToggleVertexStatus name ->
           let
               newDisplay =
                  { display
                      | graphA = toggleGlowVertex name display.graphA
                  }
            in
            VertexCover newDisplay

       VertexClicked name ->
           let
               newDisplay =
                  { display
                      | graphA = toggleGlowVertex name display.graphA
                  }
            in
            VertexCover newDisplay

       _ ->
            VertexCover display



goColor : ColorDisplay -> Msg -> Model
goColor display msg =
   case msg of
      ColoringSelectColor color ->
         let
            newDisplay = 
               {display |
                  chosenColor = color
               }
         in
         GraphColoring newDisplay

      VertexClicked name ->
         let
            newGraph = changeColorOfVertex name display.chosenColor display.graphA 
            newDisplay = {display |
                              graphA = newGraph }
         in
         GraphColoring newDisplay

      VertexNonColor ->
         let
            whiteVertices = 
               List.map (\v ->
                  {v | color = (Color.rgb 1 1 1)})
                  display.graphA.vertices

            createEdge =
               updateEdge whiteVertices

            newGraph = Graph whiteVertices (List.map createEdge display.graphA.edges)

            newDisplay = {display |
                              graphA = newGraph }
         in
         GraphColoring newDisplay

      _ ->
         GraphColoring display
             

changeColorOfVertex : Int -> Color -> Graph -> Graph
changeColorOfVertex name color graph =
   let 
      newVertices =
         List.map (\v ->
                     if v.name == name
                     then
                        {v | color = color } 
                     else
                        v)
                  graph.vertices

      createEdge =
         updateEdge newVertices
   in
   Graph newVertices (List.map createEdge graph.edges)

treeWidthDisplay : TreeWidthDisplay
treeWidthDisplay =
   let
      circularSize = vec3 100 100 0
      circularPosition = vec3 200 200 0
      circularStartAngle = 0
      gridCircularInitial =
         parametricPolygon 
            12
            circularSize
            circularPosition
            circularStartAngle

      shuffleSet =
         [ 9,8,11,12,10,7,3,1,2,4,5,6]

      gridCircular =
         gridCircularInitial
         |> List.map2 (\x y -> (x,y)) shuffleSet
         |> List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2))
         |> List.map (Tuple.second)

      gridHoneyComb = treeWidthGrid

      edgeTuples =
         [  (1, 2), (1, 3)
         ,  (2, 3), (2, 4)
         ,  (3, 4), (3, 7)
         ,  (4, 7), (4, 8), (4, 5)
         ,  (5, 6), (5, 8), (5, 9)
         ,  (6, 9)
         ,  (7, 8), (7, 10), (7, 11)
         ,  (8, 9), (8, 11)
         ,  (10, 11), (10, 12)
         ,  (11, 12)
         ]

      vertices = 
         List.map3
            (\name g c -> Vertex name g c False)
            (List.range 1 12)
            (gridCircular)
            (listOfColors First 12)
      edges =
         makeEdgesWithTuples edgeTuples vertices

      triples = [ (1,2,3), (2,3,4)
                , (3,4,7), (4,5,8), (4,7,8), (5,8,9), (5,6,9)
                , (7,10,11), (7,8,11)
                , (10,11,12)
                ]

      treeLines = [ ( (1,2,3), (2,3,4) )
                  , ( (2,3,4), (3,4,7) )
                  , ( (3,4,7), (4,7,8) )
                  , ( (4,7,8), (4,8,5) )
                  , ( (4,8,5), (5,8,9) )
                  , ( (5,8,9), (5,6,9) )
                  , ( (4,7,8), (7,8,11) )
                  , ( (7,8,11), (7,10,11) )
                  , ( (7,10,11), (10,11,12) )
                  ]

      graph = Graph vertices edges

      in
      { graph = graph 
      , gridHoneyComb = gridHoneyComb
      , gridCircular = gridCircular
      , triples = triples
      , treeLines = treeLines
      , status = CircularGraph
      }


vertexCoverDisplay : VertexCoverDisplay     
vertexCoverDisplay = 
   let
        initialGraph =
            makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)
   in
      VertexCoverDisplay initialGraph

colorDisplay : ColorDisplay
colorDisplay = 
   let
      initialGraph =
         makeGraph (PolygonCycleDoll 5) (vec3 200 100 0) (vec3 80 80 0) (pi /4)
         --makeGraph (PolygonCycleDoll 5) (vec3 200 130 0) (vec3 80 80 0) (pi /4)
      whiteVertices = 
         List.map (\v ->
            {v | color = (Color.rgb 1 1 1)})
            initialGraph.vertices
      createEdge =
         updateEdge whiteVertices
      newGraph = Graph whiteVertices (List.map createEdge initialGraph.edges)
   in
      ColorDisplay newGraph (Color.rgb 1 1 1) (Color.rgb 1 1 1)


animateMaxCutCompound : Msg -> MaxCutTransition -> MaxCutTransition
animateMaxCutCompound  msg maxCutTrans =
   case msg of
      NextAnimation ->
         let
            newState =
               case maxCutTrans.state of
                  TwoCut ->
                     ThreeCut
                  ThreeCut ->
                     TwoCut
         in
         { maxCutTrans
            | state = newState
         }

      _ ->

        case maxCutTrans.state of
           TwoCut ->
              let 
                  shapeTrans =
                     animateMaxCutTransition msg maxCutTrans.transitionA
              in
              {  maxCutTrans
                 | transitionA = shapeTrans
              }

           ThreeCut ->
              let 
                  shapeTrans =
                     animateMaxCutTransition msg maxCutTrans.transitionB
              in
              {  maxCutTrans
                 | transitionB = shapeTrans
              }

animateMaxCutTransition : Msg -> ShapeTransition -> ShapeTransition
animateMaxCutTransition  msg shapeTransition =
   case msg of
       TimeDelta delta ->
           case shapeTransition.animationOn of
               True ->
                   executeShapeTransition shapeTransition
               False ->
                   shapeTransition

       AnimationToggle ->
           { shapeTransition
               | animationOn = not shapeTransition.animationOn
           }

       AnimationStartOver ->
           { shapeTransition
               | graphB = shapeTransition.graphA
           }

       MaxCutLine ->
           { shapeTransition
               | specialToken = toggleToken shapeTransition.specialToken
           }


       _ ->
           shapeTransition

toggleToken : Token -> Token
toggleToken token =
   case token of
      MakeKCut ->
         NoToken
      NoToken ->
         MakeKCut

animateIsomorphicTransition : Msg -> ShapeTransition -> ShapeTransition
animateIsomorphicTransition msg shapeTransition =
   case msg of
       TimeDelta delta ->
           case shapeTransition.animationOn of
               True ->
                   executeShapeTransition shapeTransition
               False ->
                   shapeTransition
   
       HoverOver name ->
           { shapeTransition
               | graphA = changeGlowVertex True name <| makeUnglowAllVertices shapeTransition.graphA
               , graphB = changeGlowVertex True name <| makeUnglowAllVertices shapeTransition.graphB
           }
   
       MouseOut name ->
           { shapeTransition
             | graphA = changeGlowVertex False name shapeTransition.graphA
             , graphB = changeGlowVertex False name shapeTransition.graphB
           }
   
       ToggleVertexStatus name ->
           { shapeTransition
               | graphA = toggleGlowVertex name <| makeUnglowAllVerticesBut name shapeTransition.graphA
               , graphB = toggleGlowVertex name <| makeUnglowAllVerticesBut name shapeTransition.graphB
           }
   
       AnimationToggle ->
           { shapeTransition
               | animationOn = not shapeTransition.animationOn
           }
   
       AnimationStartOver ->
           { shapeTransition
               | graphB = shapeTransition.graphA
           }

       Other ->
           shapeTransition

       NextTopic ->
           shapeTransition
       _ ->
           shapeTransition




executeShapeTransition : ShapeTransition -> ShapeTransition
executeShapeTransition shapeTransition =
   if (distanceBetweenGraphAndGrid shapeTransition.graphB shapeTransition.finalGrid < 10)
   then { shapeTransition 
            | animationOn = False
            , graphB = morphGraph shapeTransition.graphB shapeTransition.finalGrid
        }
   else { shapeTransition 
            | graphB = moveTowards shapeTransition.graphB shapeTransition.finalGrid
        }


distanceBetweenGraphAndGrid : Graph -> Grid -> Float
distanceBetweenGraphAndGrid graph grid =
   let
      listOfDistances =
         List.map2 (\ver pos -> distance pos ver.pos) graph.vertices grid
   in
      List.sum listOfDistances


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


view model =
   case model of
      Isomorphic shapeTransition ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ELE.width ELE.fill
                  --, Background.color <| ELE.rgb 44 44 44
                  ]

                  [ displayColumn (paneOne shapeTransition.graphA shapeTransition.graphB)
                  , explanationOne shapeTransition
                  ]
            )


      MaxCut maxCutTrans ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneTwo maxCutTrans) 
                  , explanationTwo maxCutTrans.transitionA
                  ]
            )

      GraphColoring display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneThree display) 
                  , explanationColoring display
                  ]
            )

      VertexCover display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneFour display) 
                  , explanationCover display
                  ]
            )

      TreeWidth display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneTree display) 
                  , explanationWidth display
                  ]
            )



-- Change the glow status of a Vertex
-- This function changes the glow status of the Vertex to True or False
-- depending on the input status given.


changeGlowVertex : Bool -> Int -> Graph -> Graph
changeGlowVertex status name graph =
    let
        new_vertices vs =
            case vs of
                [] ->
                    []

                x :: xs ->
                    (if x.name == name then
                        { x | glow = status }

                     else
                        x
                    )
                        :: new_vertices xs
    in
    Graph (new_vertices graph.vertices) graph.edges

makeUnglowAllVerticesBut : Int -> Graph -> Graph
makeUnglowAllVerticesBut name graph =
    let
        new_vertices vs =
            case vs of
                [] ->
                    []

                x :: xs ->
                    (if x.name == name then
                        x
                     else
                        { x | glow = False}
                    )
                        :: new_vertices xs
    in
    Graph (new_vertices graph.vertices) graph.edges

makeUnglowAllVertices : Graph -> Graph
makeUnglowAllVertices graph =
    let
        new_vertices vs =
            case vs of
                [] ->
                    []

                x :: xs ->
                        { x | glow = False} :: new_vertices xs
    in
    Graph (new_vertices graph.vertices) graph.edges

toggleGlowVertex : Int -> Graph -> Graph
toggleGlowVertex  name graph =
    let
        new_vertices vs =
            case vs of
                [] ->
                    []

                x :: xs ->
                    (if x.name == name then
                        { x | glow = not (x.glow) }

                     else
                        x
                    )
                        :: new_vertices xs
    in
    Graph (new_vertices graph.vertices) graph.edges


-- Move a graph towards grid points.
-- This function takes as inputs a graph, and a target grid
-- First an intermediate grid is created near the input graph, which has the placeholder for
-- the vertices a little bit towards the final destination.
-- then the original graph is morphed into the intermediate grid.


moveTowards : Graph -> Grid -> Graph
moveTowards graph grid =
    let
        intermediateGrid =
            calcNextGrid graph grid 100
    in
    morphGraph graph intermediateGrid



-- Calculating Grid Points for Kinematics.
-- This function takes in a graph, a grid (set of 3d vectors) and creates a grid which is
-- in between the graph and the final destination.


calcNextGrid : Graph -> Grid -> Float -> Grid
calcNextGrid graph grid time =
    List.map2 (advanceVertexTowardsPosition time) graph.vertices grid



-- Calculation of Intermediate Vector Grid Point.
-- First a vector between inital Vertex and final grid point is calculated.
-- Then it is scaled down by the number of steps.
-- Finally it is added to the position vector of the the vertex.


advanceVertexTowardsPosition : Float -> Vertex -> Vec3 -> Vec3
advanceVertexTowardsPosition time vertex position =
    let
        dif =
            sub position vertex.pos
    in
      Math.Vector3.add vertex.pos (Math.Vector3.scale (1 / time) dif)


type alias Pos =
    { x : Int, y : Int }


type alias Size =
    Int



-- Scalable vector graphics

displaySvg elements =
    S.svg
        --[ SA.width "100%"
        --, SA.height "auto"
        [ SA.viewBox "0 0 400 400"
        ]
        elements





type alias Vertex =
    { name : Int, pos : Vec3, color : Color, glow : Bool }


type alias Edge =
    { vertexOne : Vertex, vertexTwo : Vertex }


type alias Graph =
    { vertices : List Vertex, edges : List Edge }



-- Polygonal Graph Types
-- Type created to aid creation of simple shapes
-- used by makeGrah function.
-- Even though a Graph can be created just by invoking the data constructor
-- Graph, without the need of Gtype or makeGraph function.
-- A graphs shape can be Polygon Cycle, in which it is created as cyclical graph with vertices
-- taking the place of vertices of a geometric polygon.
-- PolygonFullyConnected is the same as the previous one, but it is a fully connected graph. i.e. all the vertices
-- of this graph is connected to each other
-- PolygonCycleDoll will create polygonal Russian dolls with cycles and spokes when given the number of
-- vertices of the outer or inner doll. They have the same number of vertices anyway.
-- each vertex is named Ints for simplicity.


type Gtype
    = PolygonCycle Int
    | PolygonFullyConnected Int
    | PolygonCycleDoll Int



-- Make Polygonal Graphs.
-- Symplifying the creation of makeing polygonal graphs.
-- makeGraph takes a Gtype, position of the graph, size of the graph (radius of
-- the circumcribing circle), and inital orientation of the first vertex.


makeGraph : Gtype -> Vec3 -> Vec3 -> Float -> Graph
makeGraph graphType position size initialAngle =
    case graphType of
        PolygonCycle n ->
            let
                vertices =
                    List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
            in
            Graph vertices (List.map2 Edge vertices (shiftListCycle vertices))

        PolygonFullyConnected n ->
            let
                vertices =
                    List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
            in
            Graph vertices (fullyConnectVertices vertices)

        PolygonCycleDoll n ->
            let
                verticesSetA =
                    List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)

                verticesSetB =
                    List.map3 (\name g c -> Vertex name g c False)
                        (List.range (n + 1) (2 * n))
                        (parametricPolygon n (Math.Vector3.scale 0.5 size) position initialAngle)
                        (listOfColors Second n)

                allVertices =
                    verticesSetA ++ verticesSetB

                edgesCycleSetA =
                    List.map2 Edge verticesSetA (shiftListCycle verticesSetA)

                edgesCycleSetB =
                    List.map2 Edge verticesSetB (shiftListCycle verticesSetB)

                spokesSetASetB =
                    List.map2 Edge verticesSetA verticesSetB
            in
            Graph allVertices (edgesCycleSetA ++ edgesCycleSetB ++ spokesSetASetB)



-- Grid.
-- A grid is a list of positions. This type is used to provide positions for vertices
-- for graph formation or a geometry a graph can morph into by situating it's vertices on
-- the positions contained in the grid.


type alias Grid =
    List Vec3



--## Morphing a graph using Grid.
-- This function changes display geometry of a graph according to an input grid (list of vector positions).
-- while preserving the topology.
-- Vertices are reconstructed with the new positions. Edges are created between
-- the new vertices according to the incidence of original edges.


morphGraph : Graph -> Grid -> Graph
morphGraph graph grid =
    let
        updatedVertices =
            List.map2 updatePositionVertex graph.vertices grid

        createEdge =
            updateEdge updatedVertices

        updatedEdges =
            List.map createEdge graph.edges
    in
    Graph updatedVertices updatedEdges



-- a vertex is generated with the same name colour and glow but different position.


updatePositionVertex : Vertex -> Vec3 -> Vertex
updatePositionVertex ver position =
    Vertex ver.name position ver.color ver.glow



--## Put the Edges back.
--We are in a way creating a new graph, but it has the vertices of the same name, color but differnt positions
--For the edges to be the same as before
--updateEdge takes an (old edge) and a (new list of vertices)
--then produces a Edge with new vertices according to the old Edge


updateEdge : List Vertex -> Edge -> Edge
updateEdge vs e =
    let
        v1 =
            e.vertexOne

        v2 =
            e.vertexTwo
    in
    case ( lookUpVertex v1.name vs, lookUpVertex v2.name vs ) of
        ( Nothing, _ ) ->
            Edge v1 v2

        ( _, Nothing ) ->
            Edge v1 v2

        ( Just ver1, Just ver2 ) ->
            Edge ver1 ver2


lookUpVertex : Int -> List Vertex -> Maybe Vertex
lookUpVertex name vs =
    case vs of
        [] ->
            Nothing

        x :: xs ->
            if name == x.name then
                Just x

            else
                lookUpVertex name xs




-- Will connect 1 to 3,4,5,6
-- Then in next call 2 ot 3,4,5,6
-- and so on recursively


fullyConnectVertices : List Vertex -> List Edge
fullyConnectVertices vs =
    case vs of
        [] ->
            []

        x :: [] ->
            []

        x :: xs ->
            List.map (Edge x) xs ++ fullyConnectVertices xs


linearConnectVertices : List Vertex -> List Edge
linearConnectVertices vs =
    case vs of
        [] ->
            []

        [ x ] ->
            []

        x :: y :: xs ->
            Edge x y :: linearConnectVertices (y :: xs)



-- The head becomes the last element
-- and the second element becomes head


shiftListCycle xs =
    case List.tail xs of
        Just ys ->
            case List.head xs of
                Just h ->
                    ys ++ [ h ]

                Nothing ->
                    []

        Nothing ->
            []


drawVertex : Vertex -> S.Svg Msg
drawVertex v =
    circle 10 v.pos v.color v.name


drawGoldenCircle : Vertex -> S.Svg Msg
drawGoldenCircle v =
    ccircle 15 v.pos "#BF8915" v.name


drawSelectedVertex : Vertex -> S.Svg Msg
drawSelectedVertex  v =
    ccircle 13 v.pos "#BF8915" v.name

writeVertexName : Vertex -> S.Svg Msg
writeVertexName v =
    writeName v.name v.pos


drawEdge : Edge -> S.Svg msg
drawEdge e =
    line e.vertexOne.pos e.vertexTwo.pos


drawSpecialEdge : Edge -> S.Svg msg
drawSpecialEdge e =
    lline e.vertexOne.pos e.vertexTwo.pos



-- put edges first and then vertices
-- and produces a single list


drawGraph g =
    let
        ( specialEdges, normalEdges ) =
            seperateEdges g

        haloVertices =
            getHaloVertices g specialEdges

        selectedVertices =
            List.filter (\ver -> ver.glow) g.vertices
         
    in
    List.map drawEdge normalEdges
        ++ List.map drawSpecialEdge specialEdges
        ++ List.map drawGoldenCircle haloVertices
        ++ List.map drawVertex g.vertices
        ++ List.map drawSelectedVertex selectedVertices
        ++ List.map writeVertexName g.vertices


drawGraphForTreeWidth display =
   let
      g = display.graph

      centersOftriples =
         case display.status of
            TreeDrawnGraph ->
               List.filterMap 
                  (  \(a, b, c) -> findCenterOfTriple a b c g.vertices ) 
                  display.triples
            PiecesMarked ->
               List.filterMap 
                  (  \(a, b, c) -> findCenterOfTriple a b c g.vertices ) 
                  display.triples
            _ ->
               []

      treeLinesDrawn =
         case display.status of
            TreeDrawnGraph ->
               display.treeLines
               |> List.filterMap (findTwoPositions g.vertices)
            _ ->
               []

      showPieceVertices =
            case display.status of
               ShowOnePiece ->
                  List.filterMap (\name -> lookUpVertex name g.vertices)
                     [1,2,3]
               _ ->
                  []

      showPieceEdges =
            case display.status of
               ShowOnePiece ->
                     makeEdgesWithTuples [ (1,2), (2,3), (3,1) ] g.vertices
               _ ->
                 []

      onePieceCenter =
            case display.status of
               ShowOnePiece ->
                  case (findCenterOfTriple 1 2 3 g.vertices) of
                     Nothing ->
                        []
                     Just x ->
                        [x]
               _ ->
                  []
               

   
   in
   List.map drawEdge g.edges
        ++ List.map (\(p1, p2) -> lline p1 p2) treeLinesDrawn
        ++ List.map (drawIntersectionPoint 6) centersOftriples 
        ++ List.map (drawIntersectionPoint 6) onePieceCenter 
        ++ List.map drawVertex g.vertices
        ++ List.map drawSpecialEdge showPieceEdges
        ++ List.map drawSelectedVertex showPieceVertices
        ++ List.map writeVertexName g.vertices

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

      

drawGraphForCover g =
    let
        ( specialEdges, normalEdges ) =
            seperateEdges g

        haloVertices =
            getHaloVertices g specialEdges

        selectedVertices =
            List.filter (\ver -> ver.glow) g.vertices
         
    in
    List.map drawEdge normalEdges
        ++ List.map drawSpecialEdge specialEdges
    --    ++ List.map drawGoldenCircle haloVertices
        ++ List.map drawVertex g.vertices
        ++ List.map drawSelectedVertex selectedVertices
        ++ List.map writeVertexName g.vertices

drawGraphForColoring g =
    let
      verticesOfSameColor edge =
         edge.vertexOne.color == edge.vertexTwo.color && edge.vertexOne.color /= (Color.rgb 1 1 1)

      normalEdges = List.filter (\e -> not (verticesOfSameColor e)) g.edges 

      miscoloredEdges = List.filter (\e -> verticesOfSameColor e) g.edges 
    in
    List.map drawEdge normalEdges
        ++ List.map drawSpecialEdge miscoloredEdges
        ++ List.map drawVertex g.vertices
        ++ List.map writeVertexName g.vertices

getHaloVertices : Graph -> List Edge -> List Vertex
getHaloVertices g es =
   let
      glowingVertices = List.filter (\ver -> ver.glow) g.vertices
   in
      List.filter (\v -> not (isVertexInList v glowingVertices)) <| List.filter (\v -> isVertexInEdges v es) g.vertices

isVertexInList : Vertex -> List Vertex -> Bool
isVertexInList v vs =
   case (lookUpVertex v.name vs) of
      Just _ ->
         True
      Nothing ->
         False


isVertexInEdges : Vertex -> List Edge -> Bool
isVertexInEdges v es =
    case es of
        [] ->
            False

        x :: xs ->
            if v.name == x.vertexOne.name || v.name == x.vertexTwo.name then
                True

            else
                isVertexInEdges v xs


seperateEdges : Graph -> ( List Edge, List Edge )
seperateEdges g =
    let
        specialVertices =
            List.filter (\ver -> ver.glow) g.vertices

        specialEdges =
            List.filter (\edge -> isEdgeIn edge specialVertices) g.edges

        normalEdges =
            List.filter (\edge -> not (isEdgeIn edge specialVertices)) g.edges
    in
    ( specialEdges, normalEdges )


isEdgeIn : Edge -> List Vertex -> Bool
isEdgeIn e vs =
    let
        v1 =
            e.vertexOne

        v2 =
            e.vertexTwo
    in
    case ( lookUpVertex v1.name vs, lookUpVertex v2.name vs ) of
        ( Nothing, Nothing ) ->
            False

        ( _, _ ) ->
            True



-- To have different pallete of colour ranges


type ColorRegion
    = First
    | Second
    | Third



-- We start with a list of integers [0 .. n] then converted them to float
-- Normalized the whole list to [0 .. 1.0]
-- Map Color.hsl accepting hue from the list [1 .. 0]
-- So finally we have a list of colors


listOfColors : ColorRegion -> Int -> List Color
listOfColors region n =
    let
        firstRegion =
            List.range 0 (n - 1) |> List.map toFloat |> List.map (\x -> x / (3 * toFloat (n - 1)))
    in
    case region of
        First ->
            firstRegion |> List.map (\h -> Color.hsl h 1 0.7)

        Second ->
            List.map (\x -> x + 0.33) firstRegion |> List.map (\h -> Color.hsl h 1 0.5)

        Third ->
            List.map (\x -> x + 0.66) firstRegion |> List.map (\h -> Color.hsl h 1 0.5)


circle : Size -> Vec3 -> Color -> Int -> S.Svg Msg
circle size pos color name =
    S.circle
        [ SA.cx (String.fromInt <| round <| getX pos)
        , SA.cy (String.fromInt <| round <| getY pos)
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ Color.toCssString color ++ ";")
        , SE.onMouseOver (HoverOver name)
        , SE.onMouseOut (MouseOut name)
        , SE.onClick (VertexClicked name)
        ]
        []


ccircle : Size -> Vec3 -> String -> Int -> S.Svg Msg
ccircle size pos color name =
    S.circle
        [ SA.cx (String.fromInt <| round <| getX pos)
        , SA.cy (String.fromInt <| round <| getY pos)
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ color ++ ";")
        , SE.onMouseOver (HoverOver name)
        , SE.onMouseOut (MouseOut name)
        , SE.onClick (ToggleVertexStatus name)
        ]
        []

drawIntersectionPoint : Size -> Vec3 -> S.Svg Msg
drawIntersectionPoint size pos =
    S.circle
        [ SA.cx (String.fromInt <| round <| getX pos)
        , SA.cy (String.fromInt <| round <| getY pos)
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ "blue" ++ ";")
        ]
        []


writeName : Int -> Vec3 -> S.Svg Msg
writeName name pos =
    S.text_
        [ SA.x (String.fromInt <| round <| getX pos)
        , SA.y (String.fromInt <| round <| getY pos)
        , SA.class "small"
        , SA.fontSize "7px"
        , SA.textAnchor "middle"
        , SE.onClick (VertexClicked name)

        -- , SA.stroke "dark grey"
        ]
        [ S.text (String.fromInt name) ]



-- takes 2 positions and draw a line


line : Vec3 -> Vec3 -> S.Svg msg
line veca vecb =
    S.line
        [ SA.x1 (String.fromInt <| round <| getX veca)
        , SA.y1 (String.fromInt <| round <| getY veca)
        , SA.x2 (String.fromInt <| round <| getX vecb)
        , SA.y2 (String.fromInt <| round <| getY vecb)
        , SA.stroke "white"
        ]
        []


lline : Vec3 -> Vec3 -> S.Svg msg
lline veca vecb =
    S.line
        [ SA.x1 (String.fromInt <| round <| getX veca)
        , SA.y1 (String.fromInt <| round <| getY veca)
        , SA.x2 (String.fromInt <| round <| getX vecb)
        , SA.y2 (String.fromInt <| round <| getY vecb)
        , SA.stroke "#BF8915"
        , SA.strokeWidth "3"
        ]
        []


paneOne graphA graphB =
    displaySvg ((drawGraph graphA) ++ (drawGraph graphB))

paneTwo maxCutTrans =
   case maxCutTrans.state of
      TwoCut ->
         paneTwoA maxCutTrans.transitionA
      ThreeCut ->
         paneTwoB maxCutTrans.transitionB

paneTwoA shapeTransition =
   let
      graphA = shapeTransition.graphA
      graphB = shapeTransition.graphB
   in
   case shapeTransition.specialToken of
      MakeKCut ->
         let
            cutLine = makeCutLine shapeTransition
         in
         displaySvg ((drawGraph graphA) ++ (drawGraph graphB) ++ (drawCutLine cutLine))

      NoToken ->
         displaySvg ((drawGraph graphA) ++ (drawGraph graphB))

paneTwoB shapeTransition =
   let
      graphA = shapeTransition.graphA
      graphB = shapeTransition.graphB
   in
   case shapeTransition.specialToken of
      MakeKCut ->
         let
            cutLines = 
               makeCutLineB shapeTransition
            drawCutLines =
               cutLines
               |> List.map drawCutLine
               |> List.concat
         in
         displaySvg ((drawGraph graphB) ++ drawCutLines)

      NoToken ->
         displaySvg (drawGraph graphB)

paneThree display =
   displaySvg ((drawGraphForColoring display.graphA) ++ (colorPallete display))


paneFour display =
         displaySvg (drawGraphForCover display.graphA)

paneTree : TreeWidthDisplay -> H.Html Msg
paneTree display =
         displaySvg (drawGraphForTreeWidth display)
  
colorPallete : ColorDisplay -> List (S.Svg Msg)
colorPallete display=
   let
      sizeBig = (vec3 20 35 0)
      sizeSmall = (vec3 20 20 0)
      sizeOfColor color = if display.chosenColor == color
                then sizeBig
                else sizeSmall
      red = (Color.rgb 1 0 0)
      green = (Color.rgb 0 1 0)
      blue = (Color.rgb 0 0 1)
      squareRed = makeSquare (vec3 170 230 0) (sizeOfColor red) red
      squareGreen = makeSquare (vec3 200 230 0) (sizeOfColor green) green 
      squareBlue = makeSquare (vec3 230 230 0) (sizeOfColor blue) blue 
   in
   [squareRed, squareGreen, squareBlue]


makeSquare : Vec3 -> Vec3 -> Color -> S.Svg Msg
makeSquare pos size color =
   S.rect
      [ SA.x (String.fromInt <| round <| getX pos)
      , SA.y (String.fromInt <| round <| getY pos)
      , SA.width (String.fromInt <| round <| getX size)
      , SA.height (String.fromInt <| round <| getY size)
      , SA.style ("fill: " ++ Color.toCssString color ++ ";")
      , SE.onClick (ColoringSelectColor color)
      ]
      []

makeCutLineB shapeTransition = 
   let
      vertices =
         shapeTransition.graphB.vertices

      firstTuple = [ (4,6), (7,9), (1,3)]

      listOfTupledPosns =
         firstTuple
            |> List.filterMap (findPositionsOfTuples vertices)

      edgeLines =
         findEdgeLines shapeTransition.graphB.edges

      intersectionPoints =
         listOfTupledPosns
            |> List.map (\(p1, p2) ->
                           List.filterMap (findIntersection (p1, p2)) edgeLines)
   in
   List.map2 
         (\(p1, p2) ins ->
            CutLine p1 p2 ins)
         listOfTupledPosns 
         intersectionPoints
      
findPositionsOfTuples : (List Vertex) -> (Int, Int) -> Maybe (Vec3, Vec3)
findPositionsOfTuples vs tu =
   case tu of
      (name1, name2) ->
         case (lookUpVertex name1 vs, lookUpVertex name2 vs) of
            (Nothing, _ ) ->
               Nothing
            ( _, Nothing ) ->
               Nothing
            (Just vertexOne, Just vertexTwo) ->
               let
                  pullDown = vec3 0 20 0
                  pos1 = vertexOne.pos
                  pos2 = vertexTwo.pos
                  diff = normalize <| sub pos2 pos1
                  linePos2 = add pos2 (Math.Vector3.scale 50 diff)
                  linePos1 = sub pos1 (Math.Vector3.scale 50 diff)
                  (finalPos1, finalPos2) =
                     if (List.member vertexOne.name [1,2,3])
                        then
                           (  sub linePos1 pullDown
                           ,  sub linePos2 pullDown
                           )
                        else
                           (  add linePos1 <| Math.Vector3.scale 1.6 pullDown
                           ,  add linePos2 <| Math.Vector3.scale 1.6 pullDown
                           )
                           
               in
               Just (finalPos1, finalPos2)




makeCutLine shapeTransition =
   let
      start
         = vec3 210 155 0
      end
         = vec3 370 155 0

      setA = [1,2,3,4]

      setB = [5,6,7,8]

      edgeLines =
         findEdgeLines shapeTransition.graphB.edges

      intersectionPoints = 
         List.filterMap (findIntersection (start, end)) edgeLines

   in
   CutLine start end (intersectionPoints)

findEdgeLines : List Edge -> List (Vec3, Vec3)
findEdgeLines edges =
   case edges of
      (x::xs) ->
         (x.vertexOne.pos, x.vertexTwo.pos) :: findEdgeLines xs
      [] ->
         []

findIntersection : (Vec3, Vec3) -> (Vec3, Vec3) -> Maybe Vec3
findIntersection lineOne lineTwo =
   let
      (p11, p12) = 
         lineOne
      point1 =
         Pt.meters (getX p11) (getY p11)
      point2 =
         Pt.meters (getX p12) (getY p12)
      line1 =
         Ln.fromEndpoints (point1, point2)

      (p21, p22) =
         lineTwo
      point3 =
         Pt.meters (getX p21) (getY p21)
      point4 =
         Pt.meters (getX p22) (getY p22)
      line2 =
         Ln.fromEndpoints (point3, point4)

   in
   case (Ln.intersectionPoint line1 line2) of
      Nothing ->
         Nothing
      Just poinIn ->
           let 
               x = poinIn |> Pt.xCoordinate |> Len.inMeters

               y = poinIn |> Pt.yCoordinate |> Len.inMeters
           in
           Just (vec3 x y 0)
         

      



type CutLine =
   CutLine Vec3 Vec3 (List Vec3)

drawCutLine cutLine =
    case cutLine of
    CutLine start end l ->
      [(line start end)] ++ (drawIntersectionPoints l)

drawIntersectionPoints points =
   List.map (drawIntersectionPoint 3) points

explanationTwo : ShapeTransition -> ELE.Element Msg
explanationTwo shapeTransition=
      ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.height ELE.fill
         , ELE.spacing 20
         --, ELE.padding 40
         --, ELE.height ELE.fill
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.width ELE.fill
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Max Cut")
         ,  ELE.paragraph
               [ELE.spacing 8] 
               [ELE.text maxCutExplanation]

         ,  mediaButtons shapeTransition

         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     In the animation, the vertices are being segregated into
                     two sets, such that the number of edges passing from
                     vertices in one set to the vertices in another set is
                     more than any other way the vertices of the graph could
                     have been segregated.  In other words the problem of max
                     cut is to identify such partition of the vertices of the
                     graph that the above objective is satisfied.
                     """
               ]
           
        , Input.button
            [
              ELE.centerX
            ] 
            { onPress = Just MaxCutLine
            , label = Icons.minusOutlined [ Ant.width 70, Ant.height 50 ]
            }

        , ELE.paragraph
               []
               [ELE.text <| if shapeTransition.specialToken == MakeKCut 
                              then
                         
                                 """
                                 The Max cut line, seperates the two sets of vertices. The intersection
                                 between the cut line and the edges are shown as blue dots. As you should
                                 verify, they are 9 in number. This number is equal to number of edges from
                                 the set of vertices at the top going to the vertices at the bottom.
                                 """
                              else
                                 ""
               ]
          , lowerNavigation "Isomporphism" "Graph Coloring"
          ]

treeWidthButtons : TreeWidthStatus -> ELE.Element Msg
treeWidthButtons status =
   let
      forward =
         Input.button
            [
               Border.rounded 100
            ,  ELE.centerX
            ] 
            --{ onPress = Just NextTreeWidthAnimation
            { onPress = Just NextAnimation
            , label = Icons.forwardOutlined [ Ant.width 40, Ant.height 40 ]
            }
      backward =
         Input.button
            [
               Border.rounded 100
            ,  ELE.centerX
            ] 
            { onPress = Just PreviousTreeWidthAnimation
            , label = Icons.backwardOutlined [ Ant.width 40, Ant.height 40 ]
            }
   in
   case status of
      CircularGraph ->
         ELE.row
            [ELE.spacing 90, ELE.paddingXY 300 40]
            [ ELE.el 
               [ ELE.centerX
               ] 
               ELE.none

            , forward  
            ]
      _ ->
         ELE.row
            [ELE.spacing 90, ELE.paddingXY 300 40]
            [  backward
            ,  forward
            ]
      
       

explanationWidth : TreeWidthDisplay -> ELE.Element Msg
explanationWidth display =
    ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         --, ELE.height ELE.fill
         , ELE.spacing 20
         --, ELE.padding 40
         , ELE.height ELE.fill
         , ELE.width ELE.fill
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Tree Width")

         ,  ELE.paragraph
               [ELE.spacing 8] 
               [ELE.text treeWidthExplanation]

         ,  ELE.paragraph
               [ELE.spacing 8] 
               [ELE.text
                  """
                  Keep pressing the forward and backward buttons to navigate
                  through this demonstration.
                  """
               ]

         ,  treeWidthButtons display.status

         ]

         ++ (storyTreeWidth display.status)

         ++ [  lowerNavigation "Vertex Cover" "Isomorphism" ]
         
storyTreeWidth : TreeWidthStatus -> List (ELE.Element Msg)
storyTreeWidth status =
   let 
      para =
         (\l ->
            ELE.paragraph
               [ ELE.spacing 8 ]
               [ ELE.text l ]
         )

      firstComment =
               """
               The graph on the left seems very un-tree like.
               Lets morph it to another shape. Press forward button
               above to make it look a little different. 
               """
      secondComment =
               """
               Which is now transforming into a new graph, which is more
               tree-like visually.
               """
      honeyCombFirstComment =
               """
               The circular graph is now transformed to a honey comb like
               structure. Which is more like a tree-like structure visually.
               """
      showOnePieceComment =
               """
               The graph can now be divided into pieces. The first piece for example
               is the sub graph made up by Vertices 1, 2 and 3. This is marked by
               golden vertices and edges. To make life easier in further
               explanations, a piece will be represented by a blue dot present at the
               center of the subgraph.
               """
      piecesMarkedComment =
               """
               Similarily all the other pieces are marked by blue dots
               representing the subgraphs they are situated inside.
               """
      treeDetails =
               """
               The golden line joining the pieces is a tree as it has no
               cycles.
               """
      theoreticalComments =
               """
               The division of the graph in pieces such as these such that
               the pieces together form a tree is called tree decomposition of a
               graph. The pieces hence formed have associated a number of
               vertices. Here all the pieces have 3 vertices associated
               with them.
               """
      treeWidthDef =
               """
               Tree width of the graph is related to the maximum number of vertices
               associated with a piece. It is given by the formula:
               """
      treeWidthFormula =
               """
               Tree Width = (Maximum Number of Vertices in a piece) - 1
               """
      finalComment =
               """
               The number of vertices in all the pieces is equal to 3. Therefore the maximum
               number of vertices in any piece in the present graph is also 3.
               Hence the tree width of the graph is 3 - 1 = 2.
               """
      output =
         case status of
            CircularGraph ->
               [ firstComment ]
            MorphingIntoHoneyComb ->
               [ firstComment, secondComment ]
            HoneyCombGraph ->
               [ honeyCombFirstComment ]
            ShowOnePiece ->
               [honeyCombFirstComment, showOnePieceComment ]
            PiecesMarked ->
               [showOnePieceComment, piecesMarkedComment]
            TreeDrawnGraph ->
               [ treeDetails, theoreticalComments, treeWidthDef, treeWidthFormula, finalComment ]
   in
      List.map para output
            

explanationCover : VertexCoverDisplay -> ELE.Element Msg
explanationCover display =
    let
        selected_vertices =
            List.filter (\ver -> ver.glow) display.graphA.vertices

        noOfSelectedVertices =
            List.length selected_vertices

        ( coveredEdges, _ ) =
            seperateEdges display.graphA

        noCoveredEdges =
            List.length coveredEdges


        totalEdges =
            List.length display.graphA.edges

        totalVertices =
            List.length display.graphA.vertices

        edgesRemainig = 
            totalEdges - noCoveredEdges
        
    in
    ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.spacing 20
         , ELE.height ELE.fill
         , ELE.width ELE.fill
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Vertex Cover")

         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text vertexCoverExplanation ]

         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     In the task on the right, selecting a vertex will cover all
                     the edges incident on it. Your objective is to select the
                     minimum number of vertices such that, all the edges of the
                     graph are covered.
                     """
               ]

         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     To select a vertex you can press, the vertex number
                     on the keyboard. To de-select, do the same again.
                     """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if noOfSelectedVertices  == 0
                                then
                                   ""
                                else
                                   "You have selected a total of "
                                   ++ (String.fromInt noOfSelectedVertices)
                                   ++ " vertices out of "
                                   ++ (String.fromInt totalVertices)
                                   ++ " vertices. "
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if noCoveredEdges == 0
                     then
                        ""
                     else
                        "You have covered a total of "
                        ++ (String.fromInt noCoveredEdges)
                        ++ " edges out of a total of "
                        ++ (String.fromInt totalEdges)
                        ++ " edges. "
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if edgesRemainig == 0
                              then
                                 if noOfSelectedVertices > 4
                                    then
                                       """
                                       You have covered all the edges.
                                       but
                                       you have done so by selecting
                                       """
                                       ++
                                       (String.fromInt noOfSelectedVertices)
                                       ++
                                       """
                                       vertices. The graph could have been covered by
                                       selecting only four! Try again to see that
                                       you can do it in just four.
                                       """
                                    else
                                       "Congratulations, you have covered all "
                                       ++ (String.fromInt noCoveredEdges)
                                       ++ " edges. "
                                       ++ "You have done so by selecting the vertices "
                                       ++ getStringFromVertices selected_vertices
                                       ++ "."
                                       ++ " Therefore a vertex cover of this graph is the set vertices "
                                       ++ getStringFromVertices selected_vertices
                                       ++ "."

                              else
                                 if edgesRemainig == totalEdges
                                 then
                                    ""
                                 else
                                    (String.fromInt edgesRemainig)
                                    ++ " edges more to be covered!"
              ]

          , lowerNavigation "Graph Coloring" "Tree Width"
       ]



explanationColoring : ColorDisplay -> ELE.Element Msg
explanationColoring colorDisp =
    let
      verticesOfSameColor edge =
         edge.vertexOne.color == edge.vertexTwo.color 
                              && edge.vertexOne.color /= (Color.rgb 1 1 1)

      miscoloredEdges = List.filter 
                           (\e -> verticesOfSameColor e) colorDisp.graphA.edges 
      coloredVertices = List.filter  
                              (\v -> v.color /= Color.rgb 1 1 1 ) 
                              colorDisp.graphA.vertices

    in
    ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         --, ELE.height ELE.fill
         , ELE.spacing 20
         --, ELE.padding 40
         , ELE.height ELE.fill
         , ELE.width ELE.fill
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Graph Coloring")
         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text coloringExplanation ]

         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text howToColor ]
         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     As a challenge you may try to color
                     the graph with only two colors and see if
                     it is feasible.
                     """
               ]
        , Input.button
            [
              ELE.centerX
            ] 
            { onPress = Just VertexNonColor
            , label = Icons.rollbackOutlined [ Ant.width 70, Ant.height 50 ]
            }

        , ELE.paragraph
               []
               [ ELE.text <| if List.isEmpty coloredVertices
                                then
                                   ""
                                else
                                   """
                                   Coloring has started.
                                   """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if List.length coloredVertices > 1 && List.length miscoloredEdges == 0
                                then
                                   "Good going! Adjacent Vertices are colored differently."
                                else
                                   """
                                   """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if List.isEmpty miscoloredEdges
                                then
                                    ""
                                else
                                    String.join " "  <| List.map miscolorText miscoloredEdges
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if List.isEmpty miscoloredEdges
                                then
                                    ""
                                else
                                   """
                                   Try another color combination.
                                   Remember the rule; No two adjacent
                                   vertices must have the same color!
                                   """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if (List.isEmpty miscoloredEdges
                                 && List.length coloredVertices 
                                    == List.length colorDisp.graphA.vertices)
                                then
                                   """
                                   Congratulations! Graph has been colored fully and correctly.
                                   i.e. No two adjacent vertices have the same color.
                                   """
                                else
                                   ""
              ]

          , lowerNavigation "Max Cut" "Vertex Cover"
          ]



miscolorText : Edge -> String
miscolorText e =
   "Vertex " ++ (String.fromInt e.vertexOne.name) 
             ++ " and vertex "
             ++ (String.fromInt e.vertexTwo.name)
             ++ " which are adjacent to each other are colored with the same color."

mediaButtons : ShapeTransition -> ELE.Element Msg
mediaButtons shapeTransition =
   ELE.row
      [ELE.spacing 90, ELE.paddingXY 300 40]
      [  playButton shapeTransition.animationOn
      ,  resetButton
      ]


playButton : Bool -> ELE.Element Msg
playButton animationOn =
   Input.button
      []
      {  onPress = Just AnimationToggle  
      ,  label = if animationOn  
                 then   Icons.pauseOutlined [ Ant.width 50, Ant.height 50 ]
                 else   Icons.caretRightOutlined [ Ant.width 50, Ant.height 50 ]
      }

resetButton : ELE.Element Msg
resetButton =
   Input.button
         []
         {  onPress = Just AnimationStartOver
         ,  label = Icons.rollbackOutlined [ Ant.width 50, Ant.height 50 ]
 
         }
                        
explanationOne : ShapeTransition -> ELE.Element Msg
explanationOne shapeTransition =
      ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.height ELE.fill
         , ELE.spacing 20
         --, ELE.padding 40
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.height ELE.fill
         , ELE.width ELE.fill
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Graph Isomorphism")
         ,  ELE.paragraph
               [ELE.spacing 8] 
               [ELE.text isomorphismExplanation]

         , mediaButtons shapeTransition


         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     Go ahead and put your mouse over a vertex of the graph. 
                     Or press a number on the keyboard corresponding to a Vertex number.
                     """
               ]

         ]

         ++  
            (makeStory shapeTransition)
         ++


         [ lowerNavigation "Tree Width" "Max Cut" ]
               

lowerNavigation : String -> String -> ELE.Element Msg
lowerNavigation leftTitle rightTitle =
   ELE.row
      [ ELE.alignBottom 
      , ELE.width ELE.fill
      , ELE.padding 20
      , ELE.spacing 20
      ]
      [
         Input.button
         [
            Border.rounded 100
         ,  ELE.alignLeft
         ] 
         { onPress = Just PreviousTopic
         , label = Icons.verticalRightOutlined [ Ant.width 40, Ant.height 40 ]
         }
         
     ,  ELE.el [ ELE.alignLeft ] <| ELE.text leftTitle    

     ,  ELE.el [ ELE.alignRight ] <| ELE.text rightTitle    
     ,
         Input.button
         [
            Border.rounded 100
         ,  ELE.alignRight
         ] 
         { onPress = Just NextTopic
         , label = Icons.verticalLeftOutlined [ Ant.width 40, Ant.height 40 ]
         }
     ]



makeStory : ShapeTransition -> List (ELE.Element Msg)
makeStory shapeTransition =
    let
        glowing_vertices =
            List.filter (\ver -> ver.glow) shapeTransition.graphB.vertices


        putyourmouse =
            """
            Go ahead and put your mouse over a vertex of the graph.
            Or press a number on the keyboard corresponding to a Vertex number.
            """

        ( specialEdges, _ ) =
            seperateEdges shapeTransition.graphB

        relatedVertices =
            getHaloVertices shapeTransition.graphB specialEdges

        connectedToThis v =
            "And connected to vertex {{ }} are the vertices " |> String.Format.value (String.fromInt <| v.name)

        whichYouCanSee =
            " Which you can see is true for both graphs."

        listOfStories =
            case glowing_vertices of
                [] ->
                    []

                x :: xs ->
                    [ "You have selected Vertex {{}}." |> String.Format.value (String.fromInt x.name)
                    , (connectedToThis x)
                        ++ getStringFromVertices relatedVertices
                    , whichYouCanSee
                    ]

        storyPara =
            List.intersperse (ELE.html <| H.br [] [])
                (List.map ELE.text
                    listOfStories
                )

        footer =
            case glowing_vertices of
               [] -> ""
               x :: xs -> 
                  """
                  You may want to visit other vertices to see that, each vertex
                  is connected to the same vertices in both graphs.
                  Inspecting each vertices connectivity with other vertices, in both graphs you can 
                  convince your self that the graphs are isomorphic to each other.
                  """
    in
      [ ELE.paragraph [] storyPara
      , ELE.paragraph [] [ELE.text footer]
      ]



getStringFromVertices : List Vertex -> String
getStringFromVertices vs =
    case vs of
        [] ->
            ""

        x :: [] ->
            String.fromInt x.name

        x :: [ y ] ->
            String.fromInt x.name ++ " and " ++ getStringFromVertices [ y ]

        x :: xs ->
            String.fromInt x.name ++ ", " ++ getStringFromVertices xs




explanationThree =
    H.div rightSideStyle
        [ H.h1 [] [ H.text "Clique" ]
        , H.p [] [ H.text cliqueExplanation ]
        ]




explanationFour =
    H.div rightSideStyle
        [ H.h1 [] [ H.text "Clique" ]
        , H.p [] [ H.text cliqueExplanation ]
        ]


pageStyle =
    [ HA.style "height" "100vh"
    ]


leftSideStyle =
    [ HA.style "float" "left"

    --, HA.style "background" "Red"
    , HA.style "width" "45%"
    , HA.style "height" "100%"
    , HA.style "padding" "30px"
    , HA.style "margin" "10px"
    ]

goBottomStyle =
   [ HA.style "position" "absolute"
   , HA.style "bottom" "10px"
   ]

explanationStyle  = 
   [ HA.style "position" "relative"
   ]

rightSideStyle =
    [ HA.style "float" "right"

    --, HA.style "background" "Blue"
--    , HA.style "postion" "relative"
    , HA.style "width" "45%"
    , HA.style "height" "100%"
    , HA.style "padding" "30px"
    , HA.style "margin" "10px"
    ]



-- rotate a vector by angle a in the x-y plane


rotateVector v a =
    let
        rotation =
            M4.rotate a (vec3 0 0 1) M4.identity
    in
    M4.transform rotation v



-- Position of vertices of a polygon related to origin.
-- Create a list of floats and make angles out of it.
--


makePolygon : Float -> Int -> List Vec3
makePolygon startAngle n =
    let
        increment =
            2 * pi / toFloat n

        initialVector =
            vec3 1 0 0

        angles =
            List.range 0 (n - 1) |> List.map ((+) startAngle << (*) increment << toFloat)
    in
    List.map (rotateVector initialVector) angles



-- makelinear takes n : int and gives a list of 3d vecs. They are 0 in x and z, but y varies form
-- 0 to 1.0. There are n such vectors.
-- * -- * -- * n times vertically
treeWidthGrid : List Vec3
treeWidthGrid =
   let
      position = vec3 100 100 0
      size = vec3 250 400 0
      presenceList =
         [  True, False, True, False, False, False, False, False
         ,  False, True, False, True, False, True, False, True
         ,  False, False, True, False, True, False, True, False
         ,  False, True, False, True, False, False, False, False
         ,  False, False, True, False, False, False, False, False
         ]
   in
   List.map2 Tuple.pair presenceList (makelinearIn2D 5 8)
      |> List.filter (\(x,y) -> x)
      |> List.map Tuple.second
      |> situateShape position size

makelinearIn2D : Int -> Int -> List Vec3
makelinearIn2D n m =
    let
        divider =
            toFloat ((Basics.max n m) - 1)

        scalar =
            (1/divider)

        xRange =
            List.range 0 (m - 1)
            |> List.map (toFloat)
    in
    List.range 0 (n - 1) 
      |> List.map toFloat
      |> List.map (List.repeat m)
      |> List.map (List.map2 (\x y -> (x,y)) xRange)
      |> List.concat
      |> List.map (\(x,y)-> vec3 x y 0)
      |> List.map (\v -> Math.Vector3.scale scalar v)

makelinear : Int -> List Vec3
makelinear n =
    let
        divider =
            toFloat (n - 1)
    in
    List.range 0 (n - 1) |> List.map (toFloat >> (\y -> y / divider)) |> List.map (\y -> vec3 0 y 0)


linearGridLeft =
    linearGrid 4 (vec3 150 250 0) (vec3 0 120 0)


linearGridRight =
    linearGrid 4 (vec3 250 250 0) (vec3 0 120 0)



-- So that our project does not become graph problem solving
-- We have the answer to the isomorphism problem here


setLeft : List Int
setLeft =
    [ 3, 8, 6, 1 ]

setInner : List Int
setInner =
    [3, 1,6,8 ]

setRight : List Int
setRight =
    [ 7, 4, 2, 5 ]

setOuter : List Int
setOuter =
    [7,4,2,5]


-- Here as set of numbers in the left and the right are being tupled with list of vertical vector grids
-- and then sorted according to index numbers
-- This gives node 1 its vector at position left of bipartite graph
-- This gives node 6 its vector at position left of bipartite graph
-- What it does is that the vector on the second position on the left grid goes to 6th on the final grid
-- Vector on the 3rd position of the left grid goes to the 8th on the final grid


bipartiteGrid =
    let
        leftTupled =
            List.map2 (\x y -> ( x, y )) setLeft linearGridLeft

        rightTupled =
            List.map2 (\x y -> ( x, y )) setRight linearGridRight

        totalGrid =
            leftTupled ++ rightTupled
    in
    List.map (\( x, y ) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) totalGrid)

innerPolygon = parametricPolygon 4 (vec3 40 40 0) (vec3 200 300 0) (pi/2)
outerPolygon = parametricPolygon 4 (vec3 80 80 0) (vec3 200 300 0) 0

starGrid =
   let
      positionsTupled =
         List.map2 (\x y -> (x,y)) (setInner ++ setOuter) (innerPolygon ++ outerPolygon )
   in
   List.map (\(x,y) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) positionsTupled)



-- situateShape which was used to scale and locate
-- the miniturised version of the linear set of vertices


linearGrid n position size =
    situateShape position size (makelinear n)



--takes a centre , and places the shape there
--takes a scaling vector, and expands according to that
--traslateTrans is a matrix which translates a vec to a center.
--scaleTrans is a matrix which magnifies vec


situateShape : Vec3 -> Vec3 -> List Vec3 -> List Vec3
situateShape position scaleVec polygon =
    let
        translateTrans =
            M4.translate position M4.identity

        scaleTrans =
            M4.scale scaleVec M4.identity
    in
    List.map (M4.transform translateTrans << M4.transform scaleTrans) polygon


parametricPolygon : Int -> Vec3 -> Vec3 -> Float -> List Vec3
parametricPolygon n scaleVec position startAngle =
    situateShape position scaleVec <| makePolygon startAngle n

