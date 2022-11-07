module Main exposing (..)

import Html as H exposing (div, h1, p, text)
import Html.Attributes as HA exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Math.Matrix4 as M4 exposing (..)
import Math.Vector3 exposing (..)

main = view "hello there"

view model =
   div [] [ H.div pageStyle [paneOne, explanationOne]
          , H.div pageStyle [explanationTwo, paneTwo]
          , H.div pageStyle [paneThree, explanationThree]
          , H.div pageStyle [explanationFour, paneFour]
          ]

type alias Pos = {x : Int, y : Int}
type alias Size = Int

anSvg =
    S.svg
     [ SA.width "100%"
     , SA.height "auto"
     , SA.viewBox "0 0 400 400"
     ]
     --[
     --  drawEdge edge1
     --, drawVertex ver1
     --, drawVertex ver2
     --]
     --(drawGraph graph1)
     (drawGraph graph3)


type alias Vertex = {name : String, pos : Pos}
type alias Edge = {vertexOne : Vertex, vertexTwo : Vertex}
type alias Graph = {vertices : List Vertex, edges : List Edge}

--vert1 : Vertex
--vert1 = { name = "a"
--       , pos = {x = 100, y = 100}
--       }
--
--vert2 : Vertex
--vert2 = { name = "b"
--       , pos = {x = 300, y = 100}
--       }
--
--vert3 : Vertex
--vert3 = { name = "c"
--       , pos = {x = 200, y = 273}
--       }
--
--vert4 : Vertex
--vert4 = { name = "d"
--       , pos = {x = 400, y = 400}
--       }
--
--edge1 : Edge
--edge1 = { vertexOne = vert1
--        , vertexTwo = vert2
--        }
--
--edge2 : Edge
--edge2 = { vertexOne = vert1
--        , vertexTwo = vert3
--        }
--
--edge3 : Edge
--edge3 = { vertexOne = vert2
--        , vertexTwo = vert3
--        }
--
--graph1 : Graph
--graph1 =
--   {
--      vertices = [vert1, vert2, vert3]
--   ,  edges = [edge1, edge2, edge3]
--   }

vert4 = Vertex "a" (Pos 200 200)

graph3 : Graph
graph3 =
   {
      vertices = listOfVertices ++ outerListOfVertices
   ,  edges = polygonalEdges ++ outerPolygonalEdges ++ spokeEdges
   }

hexagonalVertices : List Vec3
hexagonalVertices = parametricPolygon 6 (vec3 80 80 0) (vec3 200 200 0) 0

convertGeomFormat : Vec3 -> Vertex
convertGeomFormat v = Vertex " " (Pos (round <| getX v) (round <| getY v))

listOfVertices = List.map convertGeomFormat hexagonalVertices
outerListOfVertices = List.map convertGeomFormat <| parametricPolygon 6 (vec3 120 120 0) (vec3 200 200 0) 0


polygonalEdges = List.map2 Edge listOfVertices (shiftListCycle listOfVertices)
outerPolygonalEdges = List.map2 Edge outerListOfVertices (shiftListCycle outerListOfVertices)
spokeEdges = List.map2 Edge listOfVertices outerListOfVertices

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

isomorphismExplanation =
   """
   Two graphs G1 and G2 are isomorphic if there is a one-one correspondence
   between the vertices of G1 and G2 such that the number of edges between any
   two vertices in G1 is equal to the number of edges joining the corresponding
   vertices of G2. Here the graphs may appear to be different in appearance and
   the labeling of the nodes and edges. But the way one vertex is connected to
   another in one graph is same to another. Therefore given two graphs,
   detecting if the graphs are Isomorphic is a problem to solve.  One way to
   explain this would be to manipulate the position of vertices and edges to be
   appear same as it's isomorphic counterpart.  We want to show what
   isomorphism is.
   """
hamiltonianExplanation =
   """
   Graphs containing walks (moving from one edge to another) that include every
   vertex exactly once, ending at initial vertex. (so we should start and end
   in same point without repeating vertices and cover all the vertices).
   """


cliqueExplanation =
   """
   A clique is a set of vertices of a graph such that all the vertices are
   connected to each other. This set is defined in such a way that there is no
   other vertex in the graph which can be added to the set, while preserving the
   property that all the vertices are connected to every other.
   """
rotateVector v a =
   let rotation = M4.rotate a (vec3 0 0 1) M4.identity
   in M4.transform rotation v

makePolygon : Float -> Int -> List Vec3
makePolygon startAngle n =
   let increment = (2*pi/ toFloat n)
       initialVector = vec3 1 0 0
       angles = List.range 0 (n-1) |> List.map ((+) startAngle << (*) increment << toFloat)
   in List.map (rotateVector initialVector) angles

situateShape : Vec3 -> Vec3 -> List Vec3 -> List Vec3
situateShape center scaleVec polygon=
   let translateTrans = M4.translate center M4.identity
       scaleTrans = M4.scale scaleVec M4.identity
   in
       List.map (M4.transform translateTrans << M4.transform scaleTrans) polygon

parametricPolygon : Int -> Vec3 -> Vec3 -> Float -> List Vec3
parametricPolygon n radiusVec center startAngle =
   situateShape center radiusVec <| makePolygon startAngle n
