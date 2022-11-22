module Main exposing (..)

import Browser
import Browser.Events as E
import Html.Events

import Html as H exposing (div, h1, p, text)
import Html.Events as HE exposing (..)
import Html.Attributes as HA exposing (..)

import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)

import Math.Matrix4 as M4 exposing (..)
import Math.Vector3 exposing (..)
import Explanation exposing (..)
import Color exposing (Color)

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
type alias Model =

    { graphA : Graph
    , graphB : Graph
    , finalGrid : Grid
    , animationOn : Bool
    }

-- Initializing the model
-- init function initializes, a model and provides
-- an instance of the model to the elm runtime.
init : () -> ( Model, Cmd Msg )
init _ =
   let 
     initialGraph = makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi/4)
     model = { graphA = initialGraph
             , graphB = initialGraph
             , finalGrid = bipartiteGrid 
             , animationOn = False
             }
    in
    ( model, Cmd.none )

-- Msg 
-- This data type contains the kinds of messages
-- the html page or the or a subscriber (animation clock in this app) may give to the elm-runtime.
type Msg
    = TimeDelta Float | HoverOver Int | MouseOut Int | AnimationToggle | AnimationStartOver

-- Subscribing to Animation frame clock.
-- Generates a Msg which can be used by update function
subscription : Model -> Sub Msg
subscription _ =
    E.onAnimationFrameDelta TimeDelta

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
        TimeDelta delta ->
            case model.animationOn of
               True ->
                  ( { model
                      | graphB = moveTowards model.graphB model.finalGrid
                    }
                  , Cmd.none
                  )
               False ->
                  (model, Cmd.none)

        HoverOver name ->
           ( { model
               | graphA = changeGlowVertex True name model.graphA
               , graphB = changeGlowVertex True name model.graphB
             }
           , Cmd.none)

        MouseOut name ->
           ( { model
               | graphA = changeGlowVertex False name model.graphA
               , graphB = changeGlowVertex False name model.graphB
             }
           , Cmd.none)

        AnimationToggle ->
           ( { model
               | animationOn = not (model.animationOn)
             }
           , Cmd.none)

        AnimationStartOver ->
           ( { model
             | graphB = model.graphA
             }
           , Cmd.none)


-- View the Model
-- This function is responsible for the actual rendering
-- of the webpage. Any change in the model by update function is reflected in the
-- webpage as view works with the latest model.
view model =
   div [] [ H.div pageStyle [paneOne model.graphA model.graphB, explanationOne]
          , H.div pageStyle [explanationTwo, paneTwo]
          , H.div pageStyle [paneThree, explanationThree]
          , H.div pageStyle [explanationFour, paneFour]
          ]

-- Change the glow status of a Vertex
-- This function changes the glow status of the Vertex to True or False
-- depending on the input status given.
changeGlowVertex : Bool -> Int -> Graph -> Graph
changeGlowVertex status name graph =
   let 
      new_vertices vs =
         case vs of
            [] -> []
            (x::xs) -> (if x.name == name then { x | glow = status } else x) :: (new_vertices xs)
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
      intermediateGrid = calcNextGrid graph grid 100
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
      dif = sub position vertex.pos
   in
      Math.Vector3.add (vertex.pos) (Math.Vector3.scale (1/time) dif)

type alias Pos = {x : Int, y : Int}
type alias Size = Int

-- Scalable vector graphics

anSvg =
    S.svg
     [ SA.width "100%"
     , SA.height "auto"
     , SA.viewBox "0 0 400 400"
     ]
     --(drawGraph graph3)
     --(drawGraph graph4)
     ((drawGraph graph5) ++ (drawGraph graph6) ++ (drawGraph graph7) ++ (drawGraph graph8))

anotherSvg graphA graphB =
    S.svg
     [ SA.width "100%"
     , SA.height "auto"
     , SA.viewBox "0 0 400 400"
     ]
     --(drawGraph graph3)
     --(drawGraph graph4)
     ((drawGraph graphA) ++ (drawGraph graphB))

--anotherSvg graph =
--    S.svg
--     [ SA.width "100%"
--     , SA.height "auto"
--     , SA.viewBox "0 0 400 400"
--     ]
--     --(drawGraph graph3)
--     --(drawGraph graph4)
--     ((drawGraph graph9) ++ (drawGraph transitionIntoIsomorph))

-- Vertex, Edge and Graph types.
type alias Vertex = {name : Int, pos : Vec3, color : Color, glow : Bool}
type alias Edge = {vertexOne : Vertex, vertexTwo : Vertex}
type alias Graph = {vertices : List Vertex, edges : List Edge}


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
type Gtype = PolygonCycle Int | PolygonFullyConnected Int | PolygonCycleDoll Int


-- Make Polygonal Graphs.
-- Symplifying the creation of makeing polygonal graphs.
-- makeGraph takes a Gtype, position of the graph, size of the graph (radius of
-- the circumcribing circle), and inital orientation of the first vertex.
makeGraph : Gtype -> Vec3 -> Vec3 -> Float -> Graph
makeGraph graphType position size initialAngle=
       case graphType of
          PolygonCycle n ->
             let vertices = List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
             in  Graph vertices (List.map2 Edge vertices (shiftListCycle vertices))
             
          PolygonFullyConnected n ->
             let vertices = List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
             in Graph vertices (fullyConnectVertices vertices)

          PolygonCycleDoll n ->
             let verticesSetA = List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)

                 verticesSetB = 
                     List.map3 (\name g c -> Vertex name g c False) 
                     (List.range (n+1) (2*n)) 
                     (parametricPolygon n (Math.Vector3.scale 0.5 size) position initialAngle) 
                     (listOfColors Second n)

                 allVertices = verticesSetA ++ verticesSetB
                 edgesCycleSetA = List.map2 Edge verticesSetA (shiftListCycle verticesSetA)
                 edgesCycleSetB = List.map2 Edge verticesSetB (shiftListCycle verticesSetB)
                 spokesSetASetB = List.map2 Edge verticesSetA verticesSetB
             in Graph (allVertices) (edgesCycleSetA ++ edgesCycleSetB ++ spokesSetASetB)

graph5 = makeGraph (PolygonCycle 6) (vec3 100 100 0) (vec3 50 50 0) 0
graph6 = makeGraph (PolygonFullyConnected 2) (vec3 300 100 0) (vec3 50 50 0) 0

graph7 = makeGraph (PolygonFullyConnected 3) (vec3 300 300 0) (vec3 50 50 0) 0
graph8 = makeGraph (PolygonFullyConnected 4) (vec3 100 300 0) (vec3 50 50 0) (pi/2)

graph9 = makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi/4)

-- Grid.
-- A grid is a list of positions. This type is used to provide positions for vertices
-- for graph formation or a geometry a graph can morph into by situating it's vertices on
-- the positions contained in the grid.
type alias Grid = List Vec3

--## Morphing a graph using Grid.  
-- This function changes display geometry of a graph according to an input grid (list of vector positions).
-- while preserving the topology.
-- Vertices are reconstructed with the new positions. Edges are created between
-- the new vertices according to the incidence of original edges.
morphGraph : Graph -> Grid -> Graph
morphGraph graph grid =
   let
      updatedVertices = List.map2 updatePositionVertex graph.vertices grid
      createEdge = updateEdge updatedVertices
      updatedEdges = List.map createEdge graph.edges
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
      let v1 = e.vertexOne
          v2 = e.vertexTwo
      in case ((lookUpVertex v1.name vs), (lookUpVertex v2.name vs)) of
          (Nothing, _) -> Edge v1 v2
          (_, Nothing) -> Edge v1 v2
          (Just ver1, Just ver2) -> Edge ver1 ver2


lookUpVertex : Int -> List Vertex -> Maybe Vertex
lookUpVertex name vs =
   case vs of
      [] -> Nothing
      (x::xs) -> if name == x.name then Just x else lookUpVertex name xs

-- newGraph = morphGraph graph9 (newGrid 4)

transitionIntoIsomorph = morphGraph graph9 bipartiteGrid 

newGrid n =
   let position = vec3 200 300 0
       size = vec3 80 50 0
       gridA = parametricPolygon n size position 0
       gridB = parametricPolygon n (Math.Vector3.scale 0.5 size) position 0
   in  gridA ++ gridB






-- Will connect 1 to 3,4,5,6
-- Then in next call 2 ot 3,4,5,6
-- and so on recursively
fullyConnectVertices : List Vertex -> List Edge
fullyConnectVertices vs =
   case vs of
      [] -> []
      (x :: [] ) -> []
      (x :: xs) -> (List.map (Edge x) xs) ++ fullyConnectVertices xs


linearConnectVertices : List Vertex -> List Edge
linearConnectVertices vs =
   case vs of
      [] -> []
      [x] -> []
      (x::y::xs) -> Edge x y :: linearConnectVertices (y::xs)

-- The head becomes the last element
-- and the second element becomes head
shiftListCycle xs =
   case (List.tail xs) of
      Just ys -> case (List.head xs) of
                      Just h -> ys ++ [h]
                      Nothing -> []
      Nothing -> []



drawVertex : Vertex -> S.Svg Msg
drawVertex v =
   circle 10 v.pos v.color v.name

drawGoldenCircle  : Vertex -> S.Svg Msg
drawGoldenCircle v =
   ccircle 15 v.pos "#BF8915" v.name

writeVertexName : Vertex -> S.Svg msg
writeVertexName v =
   writeText (String.fromInt v.name) v.pos

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
      (specialEdges, normalEdges) = seperateEdges g
      haloVertices = getHaloVertices g specialEdges
   in
      (List.map drawEdge normalEdges) 
      ++ (List.map drawSpecialEdge specialEdges)
      ++ (List.map drawGoldenCircle haloVertices)
      ++ (List.map drawVertex g.vertices) 
      ++ (List.map writeVertexName g.vertices)

getHaloVertices : Graph -> List Edge -> List Vertex
getHaloVertices g es =
   List.filter (\v -> isVertexInEdges v es) g.vertices

isVertexInEdges : Vertex -> List Edge -> Bool
isVertexInEdges v es =
  case es of
      [] -> False
      (x::xs) -> if (v.name == x.vertexOne.name || v.name == x.vertexTwo.name) then True else (isVertexInEdges v xs)

seperateEdges : Graph -> (List Edge, List Edge)
seperateEdges g =
   let
      specialVertices = List.filter (\ver -> ver.glow) g.vertices
      specialEdges = List.filter (\edge -> isEdgeIn edge specialVertices) g.edges
      normalEdges = List.filter (\edge -> not (isEdgeIn edge specialVertices)) g.edges
   in
      (specialEdges, normalEdges)


isEdgeIn : Edge -> List Vertex -> Bool
isEdgeIn e vs =
   let v1 = e.vertexOne
       v2 = e.vertexTwo
   in case ((lookUpVertex v1.name vs), (lookUpVertex v2.name vs)) of
      (Nothing, Nothing) -> False
      (_, _) -> True

-- To have different pallete of colour ranges
type ColorRegion = First | Second | Third

-- We start with a list of integers [0 .. n] then converted them to float
-- Normalized the whole list to [0 .. 1.0]
-- Map Color.hsl accepting hue from the list [1 .. 0]
-- So finally we have a list of colors
listOfColors : ColorRegion -> Int -> List Color
listOfColors region n = 
   let
      firstRegion = List.range 0 (n-1) |> List.map (toFloat) |> List.map (\x -> x / (3 * (toFloat (n-1))))
   in case region of
         First -> firstRegion |> List.map (\h -> Color.hsl h 1 0.7)
         Second -> List.map (\x -> x + 0.33) firstRegion |> List.map (\h -> Color.hsl h 1 0.5)
         Third -> List.map (\x -> x + 0.66) firstRegion |> List.map (\h -> Color.hsl h 1 0.5)

circle: Size -> Vec3 -> Color -> Int -> S.Svg Msg
circle size pos color name =
    S.circle
        [ SA.cx (String.fromInt <| round <| (getX pos))
        , SA.cy (String.fromInt <| round <| (getY pos))
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ (Color.toCssString color) ++ ";")
        , SE.onMouseOver (HoverOver name)
        , SE.onMouseOut (MouseOut name)
        ]
        []

ccircle: Size -> Vec3 -> String -> Int -> S.Svg Msg
ccircle size pos color name =
    S.circle
        [ SA.cx (String.fromInt <| round <| (getX pos))
        , SA.cy (String.fromInt <| round <| (getY pos))
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ color ++ ";")
        , SE.onMouseOver (HoverOver name)
        , SE.onMouseOut (MouseOut name)
        ]
        []
   

writeText: String -> Vec3 -> S.Svg msg
writeText text pos =
    S.text_
        [ SA.x (String.fromInt <| round <| (getX pos))
        , SA.y (String.fromInt <| round <| (getY pos))
        , SA.class "small"
        , SA.fontSize "7px"
        , SA.textAnchor "middle"
       -- , SA.stroke "dark grey"
        ]
        [S.text text]

-- takes 2 positions and draw a line
line : Vec3 -> Vec3 -> S.Svg msg
line veca vecb =
   S.line
      [ SA.x1 (String.fromInt <| round <| (getX veca))
      , SA.y1 (String.fromInt <| round <| (getY veca))
      , SA.x2 (String.fromInt <| round <| (getX vecb))
      , SA.y2 (String.fromInt <| round <| (getY vecb))
      , SA.stroke "white"
      ]
      []

lline : Vec3 -> Vec3 -> S.Svg msg
lline veca vecb =
   S.line
      [ SA.x1 (String.fromInt <| round <| (getX veca))
      , SA.y1 (String.fromInt <| round <| (getY veca))
      , SA.x2 (String.fromInt <| round <| (getX vecb))
      , SA.y2 (String.fromInt <| round <| (getY vecb))
      , SA.stroke "#BF8915"
      , SA.strokeWidth "3"
      ]
      []

paneOne graphA graphB = H.div leftSideStyle [ anotherSvg graphA graphB]
explanationOne = H.div rightSideStyle [ H.h1 [] [H.text "Graph Isomorphism"]
                                    , p [] [ H.text isomorphismExplanation ]
                                    , p [] [ H.button [ HE.onClick AnimationToggle ] [ H.text "Animation On/Off" ] ]
                                    , p [] [ H.button [ HE.onClick AnimationStartOver ] [ H.text "Animation Restart" ] ]
                                    ]

paneTwo = H.div rightSideStyle [anSvg]
explanationTwo = H.div leftSideStyle [ H.h1 [] [H.text "Hamiltonian Cycle"]
                                    , H.p [] [ H.text hamiltonianExplanation]
                                    ]

paneThree = H.div leftSideStyle [H.text "Graph"]
explanationThree = H.div rightSideStyle [ H.h1 [] [H.text "Clique"]
                                    , H.p [] [ H.text cliqueExplanation]
                                    ]

paneFour = H.div leftSideStyle [H.text "Graph"]
explanationFour = H.div rightSideStyle [ H.h1 [] [H.text "Clique"]
                                    , H.p [] [ H.text cliqueExplanation]
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
  
rightSideStyle =
  [ HA.style "float" "right"
  --, HA.style "background" "Blue"
  , HA.style "width" "45%"
  , HA.style "height" "100%"
  , HA.style "padding" "30px"
  , HA.style "margin" "10px"
  ]

-- rotate a vector by angle a in the x-y plane
rotateVector v a =
   let rotation = M4.rotate a (vec3 0 0 1) M4.identity
   in M4.transform rotation v

-- Position of vertices of a polygon related to origin.
-- Create a list of floats and make angles out of it.
--
makePolygon : Float -> Int -> List Vec3
makePolygon startAngle n =
   let increment = (2*pi/ toFloat n)
       initialVector = vec3 1 0 0
       angles = List.range 0 (n-1) |> List.map ((+) startAngle << (*) increment << toFloat)
   in List.map (rotateVector initialVector) angles

-- makelinear takes n : int and gives a list of 3d vecs. They are 0 in x and z, but y varies form
-- 0 to 1.0. There are n such vectors. 
-- * -- * -- * n times vertically
makelinear : Int -> List Vec3
makelinear n = 
   let
      divider = toFloat (n-1)
   in
      List.range 0 (n-1) |> List.map (toFloat >> (\y -> y/divider)) |> List.map (\y -> vec3 0 y 0)


linearGridLeft = linearGrid 4 (vec3 150 250 0) (vec3 0 120 0)
linearGridRight = linearGrid 4 (vec3 250 250 0) (vec3 0 120 0)

-- So that our project does not become graph problem solving
-- We have the answer to the isomorphism problem here
setLeft : List Int
setLeft = [3,8,6,1] 
setRight : List Int
setRight = [7,4,2,5]

-- Here as set of numbers in the left and the right are being tupled with list of vertical vector grids
-- and then sorted according to index numbers
-- This gives node 1 its vector at position left of bipartite graph
-- This gives node 6 its vector at position left of bipartite graph
-- What it does is that the vector on the second position on the left grid goes to 6th on the final grid
-- Vector on the 3rd position of the left grid goes to the 8th on the final grid
bipartiteGrid = 
   let
      leftTupled = List.map2 (\x y -> (x, y)) setLeft linearGridLeft 
      rightTupled = List.map2 (\x y -> (x, y)) setRight linearGridRight 
      totalGrid = leftTupled ++ rightTupled
   in
      List.map (\(x,y) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) totalGrid)

  

-- situateShape which was used to scale and locate
-- the miniturised version of the linear set of vertices
linearGrid n position size =
      situateShape position size (makelinear n)



--takes a centre , and places the shape there
--takes a scaling vector, and expands according to that
--traslateTrans is a matrix which translates a vec to a center.
--scaleTrans is a matrix which magnifies vec
situateShape : Vec3 -> Vec3 -> List Vec3 -> List Vec3
situateShape position scaleVec polygon=
   let translateTrans = M4.translate position M4.identity
       scaleTrans = M4.scale scaleVec M4.identity
   in
       List.map (M4.transform translateTrans << M4.transform scaleTrans) polygon

parametricPolygon : Int -> Vec3 -> Vec3 -> Float -> List Vec3
parametricPolygon n scaleVec position startAngle =
   situateShape position scaleVec <| makePolygon startAngle n
