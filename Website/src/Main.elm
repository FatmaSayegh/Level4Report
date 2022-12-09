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
   | MaxCut ShapeTransition
   | GraphColoring ColorDisplay
   | VertexCover VertexCoverDisplay

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

maxcutTransition : ShapeTransition
maxcutTransition =
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
    | ToggleTopic
    | MaxCutLine
    | ColoringSelectColor Color
    | VertexNonColor
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
                         ToggleTopic

                     'l' ->
                         MaxCutLine 
                     'w' ->
                         VertexNonColor
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
      ToggleTopic ->
         case model of
            Isomorphic x ->
               ( MaxCut maxcutTransition, Cmd.none)
            MaxCut x ->
               (GraphColoring colorDisplay, Cmd.none)
            GraphColoring x ->
               ( VertexCover vertexCoverDisplay, Cmd.none)
            VertexCover x ->
               ( Isomorphic isomorphicTransition, Cmd.none)
      _ ->
         case model of
           Isomorphic shapeTransition ->
              ( Isomorphic (animateIsomorphicTransition msg shapeTransition), Cmd.none )
           MaxCut shapeTransition ->
              ( MaxCut (animateMaxCutTransition msg shapeTransition), Cmd.none )
           GraphColoring display ->
              ( goColor display msg, Cmd.none)
           VertexCover display ->
              ( goCover display msg, Cmd.none)

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
      whiteVertices = 
         List.map (\v ->
            {v | color = (Color.rgb 1 1 1)})
            initialGraph.vertices
      createEdge =
         updateEdge whiteVertices
      newGraph = Graph whiteVertices (List.map createEdge initialGraph.edges)
   in
      ColorDisplay newGraph (Color.rgb 1 1 1) (Color.rgb 1 1 1)

--isomorphicTransition : ShapeTransition
--isomorphicTransition =
--    let
--        initialGraph =
--            makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)
--    in
--        { graphA = initialGraph
--        , graphB = initialGraph
--        , finalGrid = bipartiteGrid
--        , animationOn = False
--        , specialToken = NoToken
--        }

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

       ToggleTopic ->
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

view model =
   case model of
      Isomorphic shapeTransition ->
         ELE.layoutWith 
            { options =
               [ ELE.focusStyle
                  { borderColor = Nothing
                  , backgroundColor = Nothing
                  , shadow = Nothing
                  }
                ]
            } 
            [ELE.width ELE.fill, ELE.height ELE.fill]
            ( ELE.row
                  [ ELE.width ELE.fill
                  ]

                  [ ELE.html (paneOne shapeTransition.graphA shapeTransition.graphB) 
--                  , ELE.html (explanationOne shapeTransition)
                  , explanationOne shapeTransition
                  ]
            )

      --Isomorphic shapeTransition ->
      --   div []
      --       [ H.div pageStyle [ paneOne shapeTransition.graphA shapeTransition.graphB, addFooterTo <| explanationOne shapeTransition ]
      --       ]

      MaxCut shapeTransition ->
         div []
             [ H.div pageStyle [ explanationTwo shapeTransition, paneTwo shapeTransition]
             ]

      GraphColoring display ->
         div []
             [ H.div pageStyle [ paneThree display, 
                                 explanationColoring display 
                               ]
             ]
      VertexCover display ->
         div []
             [ H.div pageStyle [ explanationCover display 
                               , paneFour display
                               ]
             ]



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
        [ SA.width "100%"
        , SA.height "auto"
        , SA.viewBox "0 0 400 400"
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
    H.div leftSideStyle [ displaySvg ((drawGraph graphA) ++ (drawGraph graphB)) ]

paneTwo shapeTransition =
   let
      graphA = shapeTransition.graphA
      graphB = shapeTransition.graphB
   in
   case shapeTransition.specialToken of
      MakeKCut ->
         let
            cutLine = makeCutLine shapeTransition
         in
         H.div leftSideStyle [ displaySvg ((drawGraph graphA) ++ (drawGraph graphB) ++ (drawCutLine cutLine)) ]

      NoToken ->
         H.div leftSideStyle [ displaySvg ((drawGraph graphA) ++ (drawGraph graphB)) ]

paneThree display =
   H.div leftSideStyle [ displaySvg ((drawGraphForColoring display.graphA) ++ (colorPallete display)) ]


paneFour display =
   H.div leftSideStyle 
         [ displaySvg (drawGraphForCover display.graphA) ]
  
colorPallete : ColorDisplay -> List (S.Svg Msg)
colorPallete display=
   let
      sizeBig = (vec3 35 20 0)
      sizeSmall = (vec3 20 20 0)
      sizeOfColor color = if display.chosenColor == color
                then sizeBig
                else sizeSmall
      red = (Color.rgb 1 0 0)
      green = (Color.rgb 0 1 0)
      blue = (Color.rgb 0 0 1)
      squareRed = makeSquare (vec3 100 300 0) (sizeOfColor red) red
      squareGreen = makeSquare (vec3 100 330 0) (sizeOfColor green) green 
      squareBlue = makeSquare (vec3 100 360 0) (sizeOfColor blue) blue 
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

explanationTwo shapeTransition=
    H.div leftSideStyle
        [ H.h1 [] [ H.text "Max Cut" ]
        , H.p [] [ H.text maxCutExplanation ]
        , p []
            [ H.button
                [ HE.onClick AnimationToggle ]
                  [ H.text
                      ((\switch ->
                          if switch then
                              "Pause Animation"

                          else
                              "Play Animation"
                       )
                          shapeTransition.animationOn
                      )
                  ]
            ]
        , p [] [ H.button [ HE.onClick AnimationStartOver ] [ H.text "Animation Restart" ] ]
        , p [] [H.text 
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
        , p []
            [ H.button
                [ HE.onClick MaxCutLine ]
                [ H.text
                    ((\token ->
                        if token == MakeKCut then
                            "Remove Max Cut Line"

                        else
                            "Put Max Cut Line "
                     )
                        shapeTransition.specialToken
                     ) 
                ]
            ]
         , p [] [ H.text (if shapeTransition.specialToken == MakeKCut
                           then
                              """
                              The Max cut line, seperates the two sets of vertices. The intersection
                              between the cut line and the edges are shown as blue dots. As you should
                              verify, they are 9 in number. This number is equal to number of edges from
                              the set of vertices at the top going to the vertices at the bottom.
                              """
                           else
                              ""
                        )
                ]
        ]
   
explanationCover : VertexCoverDisplay -> H.Html Msg
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
    H.div rightSideStyle
       [ H.h1 [] [ H.text "Vertex Cover" ]
       , p [] [ H.text vertexCoverExplanation ]
       , p [] [ H.text
                  """
                  In the task on the right, selecting a vertex will cover all
                  the edges incident on it. Your objective is to select the
                  minimum number of vertices such that, all the edges of the
                  graph are covered.
                  """
              ]
       , p [] [ H.text
                  """
                  To select a vertex you can press, the vertex number
                  on the keyboard. To de-select, do the same again.
                  """
              ]

       , p [] [ H.text 
                  (if noOfSelectedVertices == 0
                  then
                     ""
                  else
                     "You have selected a total of "
                     ++ (String.fromInt noOfSelectedVertices)
                     ++ " vertices out of "
                     ++ (String.fromInt totalVertices)
                     ++ " vertices. "
                  )
              ]
       , p [] [ H.text 
                  (if noCoveredEdges == 0
                  then
                     ""
                  else
                     "You have covered a total of "
                     ++ (String.fromInt noCoveredEdges)
                     ++ " edges out of a total of "
                     ++ (String.fromInt totalEdges)
                     ++ " edges. "
                  )
              ]
       , p [] [ H.text
                  (if edgesRemainig == 0
                  then
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
                  )
              ]
       ]

explanationColoring : ColorDisplay -> H.Html Msg
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
    H.div rightSideStyle
       (  [ H.h1 [] [ H.text "Graph Coloring" ]
          , p [] [ H.text coloringExplanation  ]
          , p [] [ H.text howToColor ]
          , p [] [ H.text
                     """
                     As a challenge you may try to color
                     the graph with only two colors and see if
                     it is feasible.
                     """
                 ]
          , p [] [ H.button
                     [ HE.onClick VertexNonColor ]
                     [ H.text "Reset Colors" ]
                 ] 
          , p [] [ H.text <|
                           if (List.isEmpty coloredVertices)
                           then 
                              "" 
                           else
                              "Coloring has started."
                              
                 ]
          , p [] [ H.text <|
                           if (List.isEmpty miscoloredEdges)
                           then
                              ""
                           else
                              String.join " "  <| List.map miscolorText miscoloredEdges
                 ]
          , p [] [ H.text <|
                           if (List.isEmpty miscoloredEdges)
                           then
                              ""
                           else
                              """
                              Try another color combination.
                              Remember the rule; No two adjacent
                              vertices must have the same color!
                              """
                 ]

          , p [] [ H.text <|
                           if (List.isEmpty miscoloredEdges
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
          ]
       )
miscolorText : Edge -> String
miscolorText e =
   "Vertex " ++ (String.fromInt e.vertexOne.name) 
             ++ " and vertex "
             ++ (String.fromInt e.vertexTwo.name)
             ++ " which are adjacent to each other are colored with the same color."

rowButtons : ShapeTransition -> ELE.Element Msg
rowButtons shapeTransition =
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
                        
--explanationOne : ShapeTransition -> H.Html Msg
explanationOne : ShapeTransition -> ELE.Element Msg
explanationOne shapeTransition =
      ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.height ELE.fill
         , ELE.spacing 20
         , ELE.padding 40
         , ELE.height (ELE.fill |> ELE.minimum 970)
         , ELE.width ELE.fill
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Graph Isomorphism")
         ,  ELE.paragraph
               [] 
               [ELE.text isomorphismExplanation]

         , rowButtons shapeTransition


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

         [  Input.button
            [
               Border.rounded 100
            ,  ELE.alignBottom
            ,  ELE.alignRight
            ] 
            { onPress = Just ToggleTopic
            , label = Icons.rightOutlined [ Ant.width 50, Ant.height 50 ]
            }

         ]
               


--explanationOne : ShapeTransition -> ELE.Element Msg
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

--makeStory : ShapeTransition -> List (H.Html Msg)
--makeStory shapeTransition =
--    let
--        glowing_vertices =
--            List.filter (\ver -> ver.glow) shapeTransition.graphB.vertices
--
--
--        putyourmouse =
--            """
--            Go ahead and put your mouse over a vertex of the graph.
--            Or press a number on the keyboard corresponding to a Vertex number.
--            """
--
--        ( specialEdges, _ ) =
--            seperateEdges shapeTransition.graphB
--
--        relatedVertices =
--            getHaloVertices shapeTransition.graphB specialEdges
--
--        connectedToThis v =
--            "And connected to vertex {{ }} are the vertices " |> String.Format.value (String.fromInt <| v.name)
--
--        whichYouCanSee =
--            " Which you can see is true for both graphs."
--
--        listOfStories =
--            case glowing_vertices of
--                [] ->
--                    []
--
--                x :: xs ->
--                    [ "You have selected Vertex {{}}." |> String.Format.value (String.fromInt x.name)
--                    , (connectedToThis x)
--                        ++ getStringFromVertices relatedVertices
--                    , whichYouCanSee
--                    ]
--
--        storyPara =
--            List.intersperse (H.br [] [])
--                (List.map H.text
--                    listOfStories
--                )
--
--        footer =
--            case glowing_vertices of
--               [] -> ""
--               x :: xs -> 
--                  """
--                  You may want to visit other vertices to see that, each vertex
--                  is connected to the same vertices in both graphs.
--                  Inspecting each vertices connectivity with other vertices, in both graphs you can 
--                  convince your self that the graphs are isomorphic to each other.
--                  """
--    in
--      [ p [] storyPara
--      , p [] [H.text footer]
--      ]


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




--paneThree =
 --   H.div leftSideStyle [ H.text "Graph" ]


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

