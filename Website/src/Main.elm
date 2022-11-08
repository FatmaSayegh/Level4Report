module Main exposing (..)

import Html as H exposing (div, h1, p, text)
import Html.Attributes as HA exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Math.Matrix4 as M4 exposing (..)
import Math.Vector3 exposing (..)
import Explanation exposing (..)

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
     ((drawGraph graph5) ++ (drawGraph graph6))


type alias Vertex = {name : String, pos : Pos}
type alias Edge = {vertexOne : Vertex, vertexTwo : Vertex}
type alias Graph = {vertices : List Vertex, edges : List Edge}


vert4 = Vertex "a" (Pos 200 200)

graph3 : Graph
graph3 =
   {
      vertices = listOfVertices ++ outerListOfVertices
   ,  edges = polygonalEdges ++ outerPolygonalEdges ++ spokeEdges
   }

type Gtype = PolygonCycle Int | PolygonFullyConnected Int

-- <| is like $ in haskell


-- makeGraph takes a Gtype, position of the graph, size of the graph (radius of
-- the circumcribing circle), and inital orientation of the first vertex.
makeGraph : Gtype -> Vec3 -> Vec3 -> Float -> Graph
makeGraph graphType position size initialAngle=
       case graphType of
          PolygonCycle n ->
             let vertices = List.map convertGeomFormat <| parametricPolygon n size position initialAngle
             in  Graph vertices (List.map2 Edge vertices (shiftListCycle vertices))
             
          PolygonFullyConnected n ->
             let vertices = List.map convertGeomFormat <| parametricPolygon n size position initialAngle
             in Graph vertices (fullyConnectVertices vertices)

graph5 = makeGraph (PolygonCycle 6) (vec3 100 200 0) (vec3 50 50 0) 0
graph6 = makeGraph (PolygonFullyConnected 6) (vec3 300 200 0) (vec3 50 50 0) 0

-- Will connect 1 to 3,4,5,6
-- Then in next call 2 ot 3,4,5,6
-- and so on recursively
fullyConnectVertices : List Vertex -> List Edge
fullyConnectVertices vs =
   case vs of
      [] -> []
      (x :: [] ) -> []
      (x :: xs) -> (List.map (Edge x) xs) ++ fullyConnectVertices xs

graph4 : Graph
graph4 =
   {
      vertices = listOfVertices
   ,  edges = fullyConnectVertices listOfVertices
   }

hexagonalVertices : List Vec3
hexagonalVertices = parametricPolygon 6 (vec3 80 80 0) (vec3 200 200 0) 0

convertGeomFormat : Vec3 -> Vertex
convertGeomFormat v = Vertex " " (Pos (round <| getX v) (round <| getY v))

listOfVertices = List.map convertGeomFormat hexagonalVertices
outerListOfVertices = List.map convertGeomFormat <| parametricPolygon 6 (vec3 120 120 0) (vec3 200 200 0) 0


-- zipping list of vertices by Edge constructor
polygonalEdges = List.map2 Edge listOfVertices (shiftListCycle listOfVertices)
outerPolygonalEdges = List.map2 Edge outerListOfVertices (shiftListCycle outerListOfVertices)

-- zipping list of vertices by Edge constructor
spokeEdges = List.map2 Edge listOfVertices outerListOfVertices

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
   circle 10 v.pos

drawEdge : Edge -> S.Svg msg
drawEdge e =
   line e.vertexOne.pos e.vertexTwo.pos

-- put edges first and then vertices
-- and produces a single list
drawGraph : Graph -> List (S.Svg msg)
drawGraph g =
   (List.map drawEdge g.edges) ++ (List.map drawVertex g.vertices)


circle : Size -> Pos -> S.Svg msg
circle size pos =
    S.circle
        [ SA.cx (String.fromInt pos.x)
        , SA.cy (String.fromInt pos.y)
        , SA.r (String.fromInt size)
        , SA.style "fill: red;"
        ]
        []

-- takes 2 positions and draw a line
line : Pos -> Pos -> S.Svg msg
line posa posb =
   S.line
      [ SA.x1 (String.fromInt posa.x)
      , SA.y1 (String.fromInt posa.y)
      , SA.x2 (String.fromInt posb.x)
      , SA.y2 (String.fromInt posb.y)
      , SA.stroke "white"
      ]
      []

paneOne = H.div leftSideStyle [ anSvg ]
explanationOne = H.div rightSideStyle [ H.h1 [] [H.text "Graph Isomorphism"]
                                    , p [] [ H.text isomorphismExplanation]
                                    ]

paneTwo = H.div rightSideStyle [H.text "Graph"]
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
