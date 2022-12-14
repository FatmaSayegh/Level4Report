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
     (drawGraph graph9) 

type alias Name = String
type alias Vertex = {name : String, position: Vec3, color: Color} 

type alias Edge = {vertexOne : Vertex, vertexTwo : Vertex}
type alias Graph = {vertices : List Vertex, edges : List Edge}
type alias ColorGraph = {vertices : List ColorVertex, edges : List Edge}
type alias GeomGraph = {vertices : List GeomVertex, edges : List Edge}
type alias Grid = List Vec3


graph3 : Graph
graph3 =
   {
      vertices = listOfVertices ++ outerListOfVertices
   ,  edges = polygonalEdges ++ outerPolygonalEdges ++ spokeEdges
   }

type Gtype = PolygonCycle Int | PolygonFullyConnected Int | PolygonCycleDoll Int

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

          -- PolygonCycleDoll will create polygonal Russian dolls with cycles and spokes when given the number of
          -- vertices of the outer or inner doll. They have the same number of vertices anyway.
          PolygonCycleDoll n ->
             let verticesSetA = List.map convertGeomFormat <| parametricPolygon n size position initialAngle
                 verticesSetB = List.map convertGeomFormat <| parametricPolygon n (Math.Vector3.scale 0.5 size) position initialAngle
                 allVertices = verticesSetA ++ verticesSetB
                 edgesCycleSetA = List.map2 Edge verticesSetA (shiftListCycle verticesSetA)
                 edgesCycleSetB = List.map2 Edge verticesSetB (shiftListCycle verticesSetB)
                 spokesSetASetB = List.map2 Edge verticesSetA verticesSetB
             in Graph (allVertices) (edgesCycleSetA ++ edgesCycleSetB ++ spokesSetASetB)

graph5 = makeGraph (PolygonCycle 6) (vec3 100 100 0) (vec3 50 50 0) 0
graph6 = makeGraph (PolygonFullyConnected 2) (vec3 300 100 0) (vec3 50 50 0) 0

graph7 = makeGraph (PolygonFullyConnected 3) (vec3 300 300 0) (vec3 50 50 0) 0
graph8 = makeGraph (PolygonFullyConnected 4) (vec3 100 300 0) (vec3 50 50 0) (pi/2)

graph9 = makeGraph (PolygonCycleDoll 6) (vec3 200 100 0) (vec3 80 80 0) 0

updatePositionVertex : Vertex -> Vec3 -> Vertex
updatePositionVertex ver position =
   Vertex ver.name position ver.color
   
updateEdge : Edge -> List Vertex -> Edge
updateEdge e vs =
   case e of
      Edge v1 v2 -> case ((lookUpVertex v1.id vs), (lookUpVertex v2.id vs) of
                       (Nothing, _) -> Edge v1 v2
                       (_, Nothing) -> Edge v1 v2
                       (Just ver1, Just ver2) -> Edge ver1 ver2

lookUpVertex : String -> List Vertex -> Maybe Vertex
lookUpVertex name vs =
   case vs of
      [] -> Nothing
      (x:xs) -> if name == x.name then Just x else lookUpVertex id xs





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


convertGeomFormat : Vec3 -> Vertex
convertGeomFormat v = Vertex " " (Pos (round <| getX v) (round <| getY v))




-- The head becomes the last element
-- and the second element becomes head
shiftListCycle xs =
   case (List.tail xs) of
      Just ys -> case (List.head xs) of
                      Just h -> ys ++ [h]
                      Nothing -> []
      Nothing -> []



drawVertex : Vertex -> Color -> S.Svg msg
drawVertex v color =
   circle 10 v.pos color

drawEdge : Edge -> S.Svg msg
drawEdge e =
   line e.vertexOne.pos e.vertexTwo.pos

-- put edges first and then vertices
-- and produces a single list
drawGraph g =
   let
      colors = listOfColors (List.length g.vertices)
   in
      (List.map drawEdge g.edges) ++ (List.map2 (\v c -> drawVertex v c) g.vertices colors)



-- We start with a list of integers [0 .. n] then converted them to float
-- Normalized the whole list to [0 .. 1.0]
-- Map Color.hsl accepting hue from the list [1 .. 0]
-- So finally we have a list of colors
listOfColors : Int -> List Color
listOfColors n = List.range 0 n |> List.map (toFloat) |> List.map (\x -> x / (toFloat n)) |> List.map (\h -> Color.hsl h 1 0.5)

circle: Size -> Pos -> Color -> S.Svg msg
circle size pos color =
    S.circle
        [ SA.cx (String.fromInt pos.x)
        , SA.cy (String.fromInt pos.y)
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ (Color.toCssString color) ++ ";")
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
