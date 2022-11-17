module Main exposing (..)

import Html as H exposing (div, h1, p, text)
import Html.Attributes as HA exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Math.Matrix4 as M4 exposing (..)
import Math.Vector3 exposing (..)
import Explanation exposing (..)
import Color exposing (Color)

main = view "hello there"
-- main is the program
-- it is nothing but a static site with no animation, 
-- otherwise it will get a little more complicated
-- view takes in a dummy model

view model =
   div [] [ H.div pageStyle [paneOne, explanationOne]
          , H.div pageStyle [explanationTwo, paneTwo]
          , H.div pageStyle [paneThree, explanationThree]
          , H.div pageStyle [explanationFour, paneFour]
          ]

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

anotherSvg =
    S.svg
     [ SA.width "100%"
     , SA.height "auto"
     , SA.viewBox "0 0 400 400"
     ]
     --(drawGraph graph3)
     --(drawGraph graph4)
     ((drawGraph graph9) ++ (drawGraph transitionIntoIsomorph))


type alias Vertex = {name : Int, pos : Vec3, color : Color}
type alias Edge = {vertexOne : Vertex, vertexTwo : Vertex}
type alias Graph = {vertices : List Vertex, edges : List Edge}


type Gtype = PolygonCycle Int | PolygonFullyConnected Int | PolygonCycleDoll Int

-- <| is like $ in haskell


-- makeGraph takes a Gtype, position of the graph, size of the graph (radius of
-- the circumcribing circle), and inital orientation of the first vertex.
makeGraph : Gtype -> Vec3 -> Vec3 -> Float -> Graph
makeGraph graphType position size initialAngle=
       case graphType of
          PolygonCycle n ->
             let vertices = List.map3 Vertex (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
             in  Graph vertices (List.map2 Edge vertices (shiftListCycle vertices))
             
          PolygonFullyConnected n ->
             let vertices = List.map3 Vertex (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
             in Graph vertices (fullyConnectVertices vertices)

          -- PolygonCycleDoll will create polygonal Russian dolls with cycles and spokes when given the number of
          -- vertices of the outer or inner doll. They have the same number of vertices anyway.
          -- each vertex is named Ints for simplicity.
          PolygonCycleDoll n ->
             let verticesSetA = List.map3 Vertex (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
                 verticesSetB = List.map3 Vertex (List.range (n+1) (2*n)) (parametricPolygon n (Math.Vector3.scale 0.5 size) position initialAngle) (listOfColors Second n)
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


type alias Grid = List Vec3

--## Morphing a graph using Grid.  --Topology (vertices and edges) and name
--remain the same, display geometry changes. (That is position of the graph)
--Vertices are reconstructed with the new positions --Edges are created between
--the new vertices according to the original edges --All together a new graph is
--created but it's topology is the same as the original one.  --The new graph can
--be said to be visually morphed version of the original. But essentially the
--same graph connection wise and vertex name and
--vertex color wise.
morphGraph : Graph -> Grid -> Graph
morphGraph graph grid =
   let
      updatedVertices = List.map2 updatePositionVertex graph.vertices grid
      createEdge = updateEdge updatedVertices
      updatedEdges = List.map createEdge graph.edges
   in
      Graph updatedVertices updatedEdges

-- newGraph = morphGraph graph9 (newGrid 4)
transitionIntoIsomorph = morphGraph graph9 bipartiteGrid 

newGrid n =
   let position = vec3 200 300 0
       size = vec3 80 50 0
       gridA = parametricPolygon n size position 0
       gridB = parametricPolygon n (Math.Vector3.scale 0.5 size) position 0
   in  gridA ++ gridB




-- a vertex is generated with the same name colour but different position.
updatePositionVertex : Vertex -> Vec3 -> Vertex
updatePositionVertex ver position =
   Vertex ver.name position ver.color
   
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



drawVertex : Vertex -> S.Svg msg
drawVertex v =
   circle 10 v.pos v.color

drawEdge : Edge -> S.Svg msg
drawEdge e =
   line e.vertexOne.pos e.vertexTwo.pos

-- put edges first and then vertices
-- and produces a single list

drawGraph g =
   (List.map drawEdge g.edges) ++ (List.map drawVertex g.vertices)


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

circle: Size -> Vec3 -> Color -> S.Svg msg
circle size pos color =
    S.circle
        [ SA.cx (String.fromInt <| round <| (getX pos))
        , SA.cy (String.fromInt <| round <| (getY pos))
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ (Color.toCssString color) ++ ";")
        ]
        []

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

paneOne = H.div leftSideStyle [ anotherSvg]
explanationOne = H.div rightSideStyle [ H.h1 [] [H.text "Graph Isomorphism"]
                                    , p [] [ H.text isomorphismExplanation]
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

--lookUpVertex v1.name vs

linearGridLeft = linearGrid 4 (vec3 150 250 0) (vec3 0 120 0)
linearGridRight = linearGrid 4 (vec3 250 250 0) (vec3 0 120 0)

-- So that our project does not become graph problem solving
-- We have the answer to the isomorphism problem here
setRight : List Int
setRight = [1,6,8,3] 
setLeft : List Int
setLeft = [5,2,4,7]

-- Here as 
bipartiteGrid = 
   let
      leftTupled = List.map2 (\x y -> (x, y)) setRight linearGridLeft 
      rightTupled = List.map2 (\x y -> (x, y)) setLeft linearGridRight 
      totalGrid = leftTupled ++ rightTupled
   in
      List.map (\(x,y) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) totalGrid)

  
--totalGrid = linearGridLeft ++ linearGridRight


-- situateShape which was used to scale and locate
-- the miniturised version of the linear set of vertices
linearGrid n position size =
      situateShape position size (makelinear n)



--linearGraph : Int -> Vec3 -> Vec3 -> Graph
--linearGraph n position size = 
--   let 
--      vertices = list.map3 Vertex (list.range 1 n) (situateShape position size (makelinear n)) (listofcolors first n)
--      edges = linearconnectvertices vertices 
--   in
--      graph vertices edges
--partialgraph n = 
--   let 
--      vertices = list.map3 vertex (list.range 1 n) (situateshape (vec3 200 250 0) (vec3 0 40 0) (makelinear n)) (listofcolors first n)
--      edges = linearconnectvertices vertices 
--   in
--      graph vertices edges





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
