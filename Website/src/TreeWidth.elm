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
import FontSize exposing
               ( getFontSize
               , FontSize(..)
               , FontColor(..)
               , giveFontColor
               , emphForScreen
               , DisplaySize
               , DeviceType(..)
               )

miniTreeWidth = 
      let
         graph =
            treeWidthDisplay.graph
         grid =
            treeWidthDisplay.gridHoneyComb
      in
      Graph.morphGraph graph grid
      |> Graph.drawGraph
      |> Graph.displaySvg

explanationWidth : TreeWidthDisplay -> Bool -> DisplaySize -> ELE.Element Msg
explanationWidth display helpStatus displaySize =
    ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         --, ELE.height ELE.fill
         , ELE.spacing 20
         --, ELE.padding 40
         , ELE.height ELE.fill
         , ELE.width (ELE.fill |> ELE.maximum displaySize.width)
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.scrollbarY
         ]
         <|
         [  ELE.el
               [Font.size (getFontSize Head displaySize.deviceType), Font.heavy] 
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

         ,  treeWidthButtons display.status displaySize

         ]

         ++ (storyTreeWidth displaySize.deviceType display.status helpStatus)

         ++ [  lowerNavigation "Vertex Cover" "Isomorphism" ]
         
storyTreeWidth : DeviceType -> TreeWidthStatus -> Bool -> List (ELE.Element Msg)
storyTreeWidth deviceType status helpStatus =
   let 
      para =
         (\l ->
            ELE.paragraph
               [ ELE.spacing 8 ]
               l
         )

      emph =
         emphForScreen deviceType

      firstComment =
               [ ELE.text
                  """
                  The graph on the left seems very 
                  """
               , emph CuteGreen
                  """
                  un-tree 
                  """
               , ELE.text
                  """
                  like. Lets 
                  """
               , emph CuteGreen
                  """
                  morph 
                  """
               , ELE.text
                  """
                  it to another 
                  """
               , emph CuteGreen
                  """
                  shape. 
                  """
               , emph CuteBlue
                  """
                  Press 
                  """
               , ELE.text
                  """
                  forward button
                  above to make it look a little different. 
                  """
               ]
      secondComment =
               [ ELE.text
                  """
                  Which is now 
                  """
               , emph CuteBlue
                  """
                  transforming 
                  """
               , ELE.text
                  """
                  into a new graph, which is more
                  tree-like 
                  """
               , emph CuteBlue
                  """
                  visually.
                  """
               ]
      honeyCombFirstComment =
               [ ELE.text
                  """
                  The circular graph is now transformed to a 
                  """
               , emph CuteGreen
                  """
                  cellular
                  """
               , ELE.text
                  """
                  structure. Which is visually more like a 
                  """
               , emph CuteGreen
                  """
                  tree-like 
                  """
               , ELE.text
                  """
                  structure.
                  """
               ]
      showOnePieceComment =
               [ ELE.text
                  """
                  The graph can now be divided into 
                  """
               , emph CuteGreen
                  """
                  pieces. 
                  """
               , ELE.text
                  """
                  The first piece for example
                  is the 
                  """
               , emph CuteGreen
                  """
                  sub graph 
                  """
               , ELE.text
                  """
                  made up by Vertices 
                  """
               , emph Pink
                  """
                  1, 2  
                  """
               , ELE.text
                  """
                  and 
                  """
               , emph Pink
                  """
                  3
                  """
               , ELE.text
                  """
                  . This is marked by
                  golden vertices and edges. To make life easier in further
                  explanations, a piece will be represented by a 
                  """
               , emph CuteBlue
                  """
                  blue 
                  """
               , ELE.text
                  """
                  dot present at the
                  center of the subgraph.
                  """
               ]
      piecesMarkedComment =
               [ ELE.text
                  """
                  Similarily all the other pieces are marked by blue dots
                  representing the subgraphs they are situated inside.
                  """
               ]
      treeDetails =
               [ ELE.text
                  """
                  The 
                  golden 
                  lines 
                  """
               , ELE.text
                  """
                  joining the 
                  """
               , emph CuteGreen
                  """
                  pieces 
                  """
               , ELE.text
                  """
                  form a 
                  """
               , emph Gold
                  """
                  tree 
                  """
               , ELE.text
                  """
                  as this structure 
                  """
               , emph Pink
                  """
                  no
                  cycles.
                  """
               ]
      theoreticalComments =
               [ ELE.text
                  """
                  The division of the graph in pieces such as these such that
                  the pieces together form a tree is called 
                  """ 
               , emph CuteGreen
                  """
                  tree decomposition 
                  """
               , ELE.text
                  """
                  of a
                  graph. The pieces hence formed have associated a number of
                  vertices. Here all the pieces have 
                  """
               , emph Pink
                  """
                  3 
                  """
               , ELE.text
                  """
                  vertices associated
                  with them.
                  """
               ]
      treeWidthDef =
               [ ELE.text
                  """
                  Tree width of the graph is related to the maximum number of vertices
                  associated with a piece for a 
                  """
              , emph Pink
                  """
                  particular 
                  """
              ,  ELE.text
                  """
                  decomposition is given by the formula:
                  """
               ]
      treeWidthFormula =
               [ emph CuteGreen
                  """
                  Possibe Tree Width 
                  """
               , emph CuteBlue
                  """
                  = 
                  """
               , emph Pink
                  """
                  ( Maximum Number of Vertices in a piece ) 
                  """
               , emph CuteBlue
                  """
                  - 
                  """
               , emph CuteGreen
                  """
                  1
                  """
               ]
      finalComment =
               [ ELE.text
                  """
                  The number of vertices in all the pieces is equal to 3. Therefore the maximum
                  number of vertices in any piece in the present graph is also 3.
                  Hence the 
                  """
              , emph Pink
                  """
                  Possible
                  """
              , ELE.text
                  """
                  tree width for corresponding tree decomposition is
                  """
               , emph Pink
                  """
                  3 
                  """
               , emph CuteBlue
                  """
                  - 
                  """
               , emph CuteGreen
                  """
                  1 
                  """
               , emph CuteBlue
                  """
                  = 
                  """
               , emph Pink
                  """
                  2
                  """
               , ELE.text
                  """
                  .
                  """
               ]

      acrossDecompositions =
               [ ELE.text
                  """
                  There can be multiple tree decompositions, for which the number of pieces in the
                  larges piece is greater than 3.
                  """
               ]

      altTreeExplanation =
               [ ELE.text
                  """
                  Therefore, a tree can be decomposed
                  in 
                  """
               , emph CuteBlue
                  """
                  many ways
                  """
               , ELE.text
                  """
                  It can even be decomposed by keeping the
                  whole graph in a single piece. For the graph shown
                  in the figure the candidate tree
                  width in that case would be equal to 12 - 1 = 11.
                  """
               ]

      treeWidthDefNew =
               [
                 emph CuteBlue
                  """
                  Tree Width,
                  """
               , ELE.text
                  """
                   should be derived from such a tree decomposition
                  whose biggest piece is of the minimum size across all decompositions.
                  """
               ]

      finalJudgement =
               [ emph CuteBlue
                  """
                  Therefore, 
                  """
              , ELE.text
                  """
                  the tree width of the graph, is the one corresponding to the
                  """
               , emph CuteGreen
                  """
                  first
                  """
               , ELE.text
                  """
                  decomposition which has the maximum size of the piece equal to 3.
                  As it has the 
                  """
               , emph Pink
                  """
                  minimum 
                  """
               , ELE.text
                  """
                  size of the 
                  """
               , emph CuteGreen
                  """
                  biggest 
                  """
               , ELE.text
                  """
                  piece accross all tree
                  decompositions. So tree width of a graph can finally be
                  defined in the following way:
                  """
               ]

      letSBe =
               [ emph CuteBlue
                  "Let"
               , emph Pink
                  " S "
               , ELE.text
                  "be the smallest of the biggest piece accross all the tree decompositions"
               , emph CuteBlue
                  " in: "
               ]
                  
      
      formulaFinal =
               [ emph CuteGreen
                  """
                  Tree Width 
                  """
               , emph CuteBlue
                  """
                  = 
                  """
               , emph Pink
                  """
                  S
                  """
               , emph CuteBlue
                  """
                  - 
                  """
               , emph CuteGreen
                  """
                  1
                  """
               ]

      concludingComment =
               [ ELE.text
                  """
                  Hence, the tree width of the present graph =
                  """
               , emph CuteGreen "3 "
               , emph CuteBlue  "-"
               , emph CuteGreen " 1 "
               , emph CuteBlue "="
               , emph CuteGreen " 2 "
               , ELE.text "."
               ]
      bigPieceExplanation =
               [ emph Pink
                  """
                  However,
                  """
               , ELE.text
                  """
                  if the decomposition of a graph was done
                  by deriving 
                  """
               , emph CuteGreen
                  """
                  bigger 
                  """
               , ELE.text
                  """
                  pieces like the one shown in
                  the figure, then the 
                  """
               , emph Pink
                  """
                  candidate 
                  """
               , ELE.text
                  """
                  tree-width would
                  have been equal to 
                  """
               , emph Pink
                  """
                  4 
                  """
               , emph CuteBlue
                  """
                  - 
                  """
               , emph Pink
                  """
                  1 
                  """
               , emph CuteBlue
                  """
                  = 
                  """
               , emph Pink
                  """
                  3
                  """
               , ELE.text
                  """
                  .
                  """
               ]
      output =
         case status of
            CircularGraph ->
               [ firstComment ]
            MorphingIntoHoneyComb ->
               [firstComment, secondComment ]
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
               --, acrossDecompositions
               ]
            ShowLargePiece ->
               [ bigPieceExplanation ]
            AltTree ->
               [ bigPieceExplanation , altTreeExplanation, treeWidthDefNew , finalJudgement]
            FinalSlide ->
               [ finalJudgement, letSBe, formulaFinal, concludingComment]
            
   in
   if helpStatus == False
      then
         List.map para output
      else
         [ helpParagraph TreeWidthHelp]

treeWidthButtons : TreeWidthStatus -> DisplaySize -> ELE.Element Msg
treeWidthButtons status displaySize =
   case status of
      CircularGraph ->
         treeWidthButtonRow False displaySize
      _ ->
         treeWidthButtonRow True displaySize

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
                     ShowLargePiece
                  ShowLargePiece ->
                     AltTree
                  AltTree ->
                     FinalSlide
                  FinalSlide ->
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
                  ShowLargePiece ->
                     TreeDrawnGraph
                  AltTree ->
                     ShowLargePiece
                  FinalSlide ->
                     AltTree

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
            FinalSlide ->
               List.filterMap 
                  (  \(a, b, c) -> Graph.findCenterOfTriple a b c g.vertices ) 
                  display.triples
            _ ->
               []

      unsafeHead xs =
         case xs of
            (x :: xss) -> x
            [] -> vec3 0 0 0

      firstBranchAltTree =
         case display.status of
            AltTree ->
               let
                  firstDot =
                     bigPieceCenter
                     |> unsafeHead
                  secondPoint =
                     altTreeDots
                     |> unsafeHead
               in
               [(firstDot, secondPoint)]
            _ ->
             []
               

      treeLinesDrawn =
         case display.status of
            TreeDrawnGraph ->
               display.treeLines
               |> List.filterMap (Graph.findTwoPositions g.vertices)
            FinalSlide ->
               display.treeLines
               |> List.filterMap (Graph.findTwoPositions g.vertices)
            _ ->
               []

      unsafeTail xs =
         case xs of
            [] -> []
            [x] -> []
            (x :: y :: xss) -> xss

      altTreeLinesDrawn =
         case display.status of
            AltTree ->
               display.treeLines
               |> List.filterMap (Graph.findTwoPositions g.vertices)
               |> unsafeTail
            _ ->
               []

      altTreeDots =
         case display.status of
            AltTree ->
               List.filterMap 
                  (  \(a, b, c) -> Graph.findCenterOfTriple a b c g.vertices ) 
                  display.triples
               |> unsafeTail
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

      showBigPieceEdges =
            case display.status of
               ShowLargePiece ->
                     Graph.makeEdgesWithTuples [ (1,2), (2,3), (2,4), (4,3), (3,1) ] g.vertices
               _ ->
                 []

      showGrayEdges =
            case display.status of
               AltTree ->
                     Graph.makeEdgesWithTuples [ (1,2), (2,3), (2,4), (4,3), (3,1) ] g.vertices
               _ ->
                 []

      showGrayVertices =
            case display.status of
               AltTree ->
                  List.filterMap (\name -> Graph.lookUpVertex name g.vertices)
                     [1,2,3,4]
               _ ->
                  []

      showBigPieceVertices =
            case display.status of
               ShowLargePiece ->
                  List.filterMap (\name -> Graph.lookUpVertex name g.vertices)
                     [1,2,3,4]
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

      bigPieceCenter =
            case display.status of
               ShowLargePiece ->
                  case (Graph.findCenterOfQuad 1 2 3 4 g.vertices) of
                     Nothing ->
                        []
                     Just x ->
                        [x]

               AltTree ->
                  case (Graph.findCenterOfQuad 1 2 3 4 g.vertices) of
                     Nothing ->
                        []
                     Just x ->
                        [x]
               _ ->
                  []
               

   
   in
   List.map Graph.drawEdge g.edges
        ++ List.map (\(p1, p2) -> Graph.lline p1 p2) treeLinesDrawn
        ++ List.map Graph.drawGrayEdge showGrayEdges
        ++ List.map (\(p1, p2) -> Graph.lline p1 p2) altTreeLinesDrawn
        ++ List.map (\(p1, p2) -> Graph.lline p1 p2) firstBranchAltTree
        ++ List.map (Graph.drawIntersectionPoint 6) centersOftriples 
        ++ List.map (Graph.drawIntersectionPoint 6) altTreeDots 
        ++ List.map Graph.drawVertex g.vertices
        ++ List.map Graph.drawSpecialEdge showPieceEdges
        ++ List.map Graph.drawSpecialEdge showBigPieceEdges
        ++ List.map Graph.drawSelectedVertex showPieceVertices
        ++ List.map Graph.drawSelectedVertex showBigPieceVertices
        ++ List.map Graph.drawGrayVertex showGrayVertices
        ++ List.map (Graph.drawIntersectionPoint 6) bigPieceCenter 
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
   | ShowLargePiece
   | AltTree
   | FinalSlide
