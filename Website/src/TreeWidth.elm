module TreeWidth exposing (..)

import Graph exposing (Graph, ColorRegion(..), linearGrid, parametricPolygon, Grid, makeGraph, Gtype(..), ShapeTransition, Token(..))
import Math.Vector3 exposing (..)
import Messages exposing (Msg(..))
import Element as ELE
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Explanation exposing (..)
import Buttons exposing (..)
import String.Format
import Html as H exposing (div, h1, p, text)
import Element.Input as Input
import Ant.Icon as Ant
import Ant.Icons as Icons
import Color exposing (Color)
import Svg as S
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)

explanationWidth : TreeWidthDisplay -> Bool -> ELE.Element Msg
explanationWidth display helpStatus =
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

         ++ (storyTreeWidth display.status helpStatus)

         ++ [  lowerNavigation "Vertex Cover" "Isomorphism" ]
         
storyTreeWidth : TreeWidthStatus -> Bool -> List (ELE.Element Msg)
storyTreeWidth status helpStatus =
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
               [ treeDetails
               , theoreticalComments
               , treeWidthDef
               , treeWidthFormula
               , finalComment 
               ]
   in
   if helpStatus == False
      then
         List.map para output
      else
         [ helpParagraph TreeWidthHelp]

treeWidthButtons : TreeWidthStatus -> ELE.Element Msg
treeWidthButtons status =
   case status of
      CircularGraph ->
         treeWidthButtonRow False
      _ ->
         treeWidthButtonRow True

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
            (\name g c -> Graph.Vertex name g c False)
            (List.range 1 12)
            (gridCircular)
            (Graph.listOfColors First 12)
      edges =
         Graph.makeEdgesWithTuples edgeTuples vertices

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
      , time = 0.0
      }
goTree : TreeWidthDisplay -> Msg -> TreeWidthDisplay
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
                     Graph.morphGraph display.graph display.gridCircular
                   HoneyCombGraph ->
                     Graph.morphGraph display.graph display.gridHoneyComb
                   _ ->
                     display.graph

         in
         { display
           | status = newStatus
           , graph = newGraph
         }


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
                     Graph.morphGraph display.graph display.gridCircular
                   HoneyCombGraph ->
                     Graph.morphGraph display.graph display.gridHoneyComb
                   _ ->
                     display.graph

         in
         { display
           | status = newStatus
           , graph = newGraph
         }

      TimeDelta delta ->
         if display.status == MorphingIntoHoneyComb
            then
              morphIntoHoneyComb delta display
            else
              display
      
      _ ->
         display


morphIntoHoneyComb : Float -> TreeWidthDisplay -> TreeWidthDisplay
morphIntoHoneyComb delta display =
   if (Graph.distanceBetweenGraphAndGrid display.graph display.gridHoneyComb < 20)
      then { display
               | status = HoneyCombGraph
               , graph = Graph.morphGraph display.graph display.gridHoneyComb
               , time = 0.0
           }
      else
           let
               accumulatedTime =
                  display.time + delta

               calculatedTime =
                  delta/(2000 - accumulatedTime)
           in
           { display
               | graph = Graph.moveTowards calculatedTime display.graph display.gridHoneyComb
               , time = accumulatedTime
           }

drawGraphForTreeWidth display =
   let
      g = display.graph

      centersOftriples =
         case display.status of
            TreeDrawnGraph ->
               List.filterMap 
                  (  \(a, b, c) -> Graph.findCenterOfTriple a b c g.vertices ) 
                  display.triples
            PiecesMarked ->
               List.filterMap 
                  (  \(a, b, c) -> Graph.findCenterOfTriple a b c g.vertices ) 
                  display.triples
            _ ->
               []

      treeLinesDrawn =
         case display.status of
            TreeDrawnGraph ->
               display.treeLines
               |> List.filterMap (Graph.findTwoPositions g.vertices)
            _ ->
               []

      showPieceVertices =
            case display.status of
               ShowOnePiece ->
                  List.filterMap (\name -> Graph.lookUpVertex name g.vertices)
                     [1,2,3]
               _ ->
                  []

      showPieceEdges =
            case display.status of
               ShowOnePiece ->
                     Graph.makeEdgesWithTuples [ (1,2), (2,3), (3,1) ] g.vertices
               _ ->
                 []

      onePieceCenter =
            case display.status of
               ShowOnePiece ->
                  case (Graph.findCenterOfTriple 1 2 3 g.vertices) of
                     Nothing ->
                        []
                     Just x ->
                        [x]
               _ ->
                  []
               

   
   in
   List.map Graph.drawEdge g.edges
        ++ List.map (\(p1, p2) -> Graph.lline p1 p2) treeLinesDrawn
        ++ List.map (Graph.drawIntersectionPoint 6) centersOftriples 
        ++ List.map (Graph.drawIntersectionPoint 6) onePieceCenter 
        ++ List.map Graph.drawVertex g.vertices
        ++ List.map Graph.drawSpecialEdge showPieceEdges
        ++ List.map Graph.drawSelectedVertex showPieceVertices
        ++ List.map Graph.writeVertexName g.vertices

paneTree : TreeWidthDisplay -> H.Html Msg
paneTree display =
         Graph.displaySvg (drawGraphForTreeWidth display)

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
   List.map2 Tuple.pair presenceList (Graph.makelinearIn2D 5 8)
      |> List.filter (\(x,y) -> x)
      |> List.map Tuple.second
      |> Graph.situateShape position size

type alias TreeWidthDisplay =
   { graph : Graph
   , gridCircular : Grid
   , gridHoneyComb : Grid
   , triples : List (Int, Int, Int)
   , treeLines : List ( (Int, Int, Int), (Int, Int, Int) )
   , status : TreeWidthStatus
   , time : Float
   }

type TreeWidthStatus =
   CircularGraph
   | MorphingIntoHoneyComb
   | HoneyCombGraph
   | ShowOnePiece
   | PiecesMarked
   | TreeDrawnGraph
