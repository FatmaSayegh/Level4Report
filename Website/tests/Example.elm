module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Url
import Messages exposing (..)
import Math.Vector3 exposing (..)

import Isomorphism exposing 
   (IsomorphicTopic, 
   explanationOne, 
   paneOne, 
   animateIsomorphicTransition, 
   isomorphicTransition, 
   miniIsoGraph, 
   isomorphicTopic, 
   animateIsomorphicTopic, 
   isomorphicDisplay
   )

import MaxkCut exposing 
   (  MaxCutTransition, 
      explanationTwo, 
      paneTwo, 
      animateMaxCutCompound, 
      maxCutTransition, 
      miniMaxGraph
   )

import GraphColoring exposing 
   (  ColorDisplaySeries, 
      paneThree, 
      explanationColoring, 
      colorDisplaySeries, 
      goColorSeries, 
      miniColGraph
   )

import VertexCover exposing 
   (  VertexCoverDisplay, 
      paneFour, 
      explanationCover, 
      vertexCoverDisplay, 
      goCover
   )

import TreeWidth exposing 
   (  TreeWidthDisplay, 
      paneTree, 
      explanationWidth, 
      treeWidthDisplay, 
      goTree, 
      miniTreeWidth
   )

import Graph exposing (..)


keyToMsgTest : Test
keyToMsgTest =
   describe "testing key to msg"
      [  testKey "r" AnimationStartOver
      ,  testKey "p" AnimationToggle
      ,  testKey "n" NextTopic
      ,  testKey "1" <| ToggleVertexStatus 1
      ,  testKey "2" <| ToggleVertexStatus 2
      ,  testKey "3" <| ToggleVertexStatus 3
      ,  testKey "4" <| ToggleVertexStatus 4
      ]


testKey : String -> Msg -> Test
testKey str msg =
   test ("testing key " ++ str) <|
      \_ ->
         keyToMsg str
         |> Expect.equal msg

myGrid =
      parametricPolygon 8 (vec3 100 100 0) (vec3 150 150 0) 0

testGraph =
      makeGraph 
         (PolygonCycleDoll 4) 
         (vec3 200 100 0) 
         (vec3 80 80 0) 
         (pi / 4)

changeGlowTestSuite : Test
changeGlowTestSuite =
   describe "testing unglowing vertices"
   [ changeGlowTest True 1 testGraph 
   , changeGlowTest True 2 testGraph 
   , changeGlowTest True 3 testGraph 
   , changeGlowTest True 4 testGraph 
   , changeGlowTest True 5 testGraph 
   , changeGlowTest True 6 testGraph 
   , changeGlowTest True 7 testGraph 
   , changeGlowTest True 8 testGraph 
   , changeGlowTest False 1 testGraph 
   , changeGlowTest False 2 testGraph 
   , changeGlowTest False 3 testGraph 
   , changeGlowTest False 4 testGraph 
   , changeGlowTest False 5 testGraph 
   , changeGlowTest False 6 testGraph 
   , changeGlowTest False 7 testGraph 
   , changeGlowTest False 8 testGraph 
   ]

changeGlowTest : Bool -> Int -> Graph -> Test
changeGlowTest status name graph =
   let
      bool_string =
         case status of
            True ->
               " For True"
            False ->
               " For False"
   in
   test ("testing changing of status " ++ (String.fromInt name) ++ bool_string) <|
      \_ ->
         let
            new_graph = changeGlowVertex status name graph
            vertices = new_graph.vertices 
            vertex_with_name = 
               List.drop (name - 1) vertices
               |> List.head
         in
         case vertex_with_name of
            Just v ->
                  Expect.equal status v.glow
            Nothing ->
                  Expect.equal status (not status)


graphGridDistSuite : Test
graphGridDistSuite =
   graphGridTestAdapter 
      graphGridDistTest 
      "For dist value"

graphGridDistValueSuite : Test 
graphGridDistValueSuite  =
   graphGridTestAdapter 
      graphGridDistValueTest  
      "Should be greater than zero"

graphGridTestAdapter : (String -> Graph -> Grid -> Test) -> String -> Test
graphGridTestAdapter thatFunction name =
   describe ("testing distanceBetweenGraphAndGrid function " ++ name) <|
      let
         sizes =
            List.range 3 200
         pos =
            (vec3 100 100 0)
         sizeVec =
            (vec3 150 150 0)
         grids = 
            sizes
            |> List.map (\n -> parametricPolygon (n*2) pos sizeVec 0 )
         graphs =
            sizes
            |> List.map (\n -> makeGraph (PolygonCycleDoll n) pos sizeVec 0)

         names =
            sizes
            |> List.map (String.fromInt )
      in
      List.map3 thatFunction 
            names 
            graphs 
            grids

graphGridDistTest : String -> Graph -> Grid -> Test
graphGridDistTest name graph grid =
   test ("testing graphGridDistTest" ++ name) <|
      \_ ->
         let
            ver_pos =
               graph
               |> .vertices
               |> List.map .pos
            
            distBwGrGrid =
               distanceBetweenGraphAndGrid graph grid

            listOfDifference =
               ver_pos
               |> List.map2 (\gr ver -> distance gr ver) grid
               |> List.sum
         in
         Expect.within (Expect.Absolute 0.001) listOfDifference distBwGrGrid
                              
                              
graphGridDistValueTest : String -> Graph -> Grid -> Test
graphGridDistValueTest name graph grid =
   test ("testing graphGridDistTest is greater than zero for sizes " ++ name) <|
      \_ ->
         let
            
            distBwGrGrid =
               distanceBetweenGraphAndGrid graph grid

         in
         Expect.greaterThan 0.0 distBwGrGrid


makeGraphNoVerticesSuite : Test
makeGraphNoVerticesSuite =
   describe "Checking if correct number of vertices is made correctly " <|
      let
         polygonSizes =
            List.range 3 200

         polygonCycles =
            List.map PolygonCycle polygonSizes

         fullyConnecteds =
            List.map PolygonFullyConnected polygonSizes

         dolls =
            List.map PolygonCycleDoll polygonSizes

         combinedGtypes =
            polygonCycles ++ fullyConnecteds ++ dolls
      in
      List.map
         makeGraphNoOfVerticesTest combinedGtypes
         


makeGraphNoOfVerticesTest : Gtype -> Test
makeGraphNoOfVerticesTest gtype =
      let
         sizeOfGraph =
            (vec3 150 150 0)

         positionOfGraph =
            (vec3 150 150 0)

         graph =
            makeGraph gtype positionOfGraph sizeOfGraph 0

         (expNoVer, gtypeStr) =
            case gtype of
               PolygonCycle n ->
                  (n, "Cycle")
               PolygonFullyConnected n ->
                  (n, "Fully Connected")
               PolygonCycleDoll n ->
                  (2*n, "Doll")
      in
      test 
         ("No. of vertices according to type of graph " 
            ++ String.fromInt expNoVer
            ++ gtypeStr)  <|
         \_ ->
            graph
            |> .vertices
            |> List.length
            |> Expect.equal expNoVer

      

isLookUpVertexNameSameSuit : Test
isLookUpVertexNameSameSuit =
   describe "Checking if lookUpVertexName finds a vertex with correct name" <|
   let
      name =
         1
      polyGonVertices =
         List.range 3 200

      sizeOfGraph =
         (vec3 150 150 0)

      positionOfGraph =
         (vec3 150 150 0)

      gtypes =
         List.map
            PolygonCycle polyGonVertices

      graphs =
         gtypes
         |> List.map (\t ->
             makeGraph t sizeOfGraph positionOfGraph 0)

   in
   List.map
      (isLookUpVertexNameSame name)
      graphs

isLookUpVertexNameSame : Int -> Graph -> Test         
isLookUpVertexNameSame  name graph =
   let
      expectedName =
         name
      
      nameOfTest =
         String.fromInt <| List.length graph.vertices

      foundVertex =
         graph.vertices
         |> lookUpVertex name

      foundName =
         case foundVertex of
            Just v ->
               v.name
            Nothing ->
               0
  in
  test ("lookUpVertexName test for " ++ (String.fromInt foundName) ++ nameOfTest) <|
      \_ ->
         Expect.equal expectedName foundName
         
fullyConnectedGraphEdgeCountSuite : Test         
fullyConnectedGraphEdgeCountSuite =
   describe "fully connected graph edge count suite " <|
   let
      polySize =
         List.range 3 100
   in
   List.map
      fullyConnectedGraphEdgeCountTest         
      polySize

fullyConnectedGraphEdgeCountTest : Int -> Test         
fullyConnectedGraphEdgeCountTest n =
   let
      size = (vec3 100 100 0)

      pos = (vec3 100 100 0)

      graph =
         makeGraph
            (PolygonFullyConnected n)
            size
            pos
            0

      noOfEdges =
         graph.edges
         |> List.length

  in
  test ("fully connected graph edge count " ++ (String.fromInt n)) <|
   \_ ->
      Expect.equal 
         (( n * ( n - 1) )//2)
         noOfEdges
         
--suite =
--   describe "Tests for numerical literacy"
--      [
--         test "two plus two equals four" <|
--            \_ ->
--               (2 + 2)
--               |> Expect.equal 4
--      ,
--         test "three plus three is six" <|
--            \_ ->
--               (3 + 3)
--               |> Expect.equal 6
--      ]

--fuzzyTesting : Test
--fuzzyTesting =
--   describe "Tests for addition"
--      [
--         fuzz int "adds 1 to any integer" <|
--            \num ->
--               addOne num
--               |> Expect.equal (num + 1)
--      ,  fuzz int "another function which adds 1 to any integer" <|
--            \num ->
--               addOneCorrect num
--               |> Expect.equal (num + 1)
--      ]
--
--addOne : Int -> Int
--addOne x =
--   if x == 0
--      then
--         x
--      else
--         x + 1
--   
--addOneCorrect : Int -> Int
--addOneCorrect x =
--   x + 1