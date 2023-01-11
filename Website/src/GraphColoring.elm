module GraphColoring exposing (..)

import Graph exposing (Graph, ColorRegion(..), linearGrid, parametricPolygon, Grid, makeGraph, Gtype(..), ShapeTransition, Token(..))
import Math.Vector3 exposing (..)
import Messages exposing (Msg(..))
import Element as ELE
import Element.Background as Background
import Element.Font as Font
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
      , emph
      )

miniColGraph = 
      makeGraph (PolygonCycleDoll 5) (vec3 200 100 0) (vec3 80 80 0) (pi /4)
      |> Graph.drawGraph
      |> Graph.displaySvg

type alias ColorDisplay =
   { graphA : Graph
   , chosenColor : Color
   , defaultColor : Color
   }

type ColorDisplayState =
   TwoColor
   | ThreeColor

type alias ColorDisplaySeries =
   { colorDisplayA : ColorDisplay
   , colorDisplayB : ColorDisplay
   , state : ColorDisplayState
   }

colorDisplaySeries : ColorDisplaySeries
colorDisplaySeries =
   { colorDisplayA = colorDisplayA
   , colorDisplayB = colorDisplayB
   , state = TwoColor
   }

colorDisplayB : ColorDisplay
colorDisplayB = 
   let
      initialGraph =
         makeGraph (PolygonCycleDoll 5) (vec3 200 100 0) (vec3 80 80 0) (pi /4)
         --makeGraph (PolygonCycleDoll 5) (vec3 200 130 0) (vec3 80 80 0) (pi /4)
      whiteVertices = 
         List.map (\v ->
            {v | color = (Color.rgb 1 1 1)})
            initialGraph.vertices
      createEdge =
         Graph.updateEdge whiteVertices
      newGraph = Graph whiteVertices (List.map createEdge initialGraph.edges)
   in
      ColorDisplay newGraph (Color.rgb 1 1 1) (Color.rgb 1 1 1)

colorDisplayA : ColorDisplay
colorDisplayA = 
   let
            
      linearGridLeft =
          --linearGrid 4 (vec3 150 250 0) (vec3 0 120 0)
          linearGrid 4 (vec3 150 50 0) (vec3 0 120 0)
      
      linearGridRight =
          --linearGrid 4 (vec3 250 250 0) (vec3 0 120 0)
          linearGrid 4 (vec3 250 50 0) (vec3 0 120 0)

      totalGrid =
          linearGridLeft ++ linearGridRight

      vertices =
         List.map3 
            (\name g c -> Graph.Vertex name g c False) 
            (List.range 1 8) 
            totalGrid
            (Graph.listOfColors First 8)

      tupleEdges =
         [ (1,5), (1,6), (1,7)
         , (2,5), (2,6), (2,8)
         , (3,5), (3,7), (3,8)
         , (4,6), (4,7), (4,8)
         ]

      --edges =
      --   Graph.makeEdgesWithTuples tupleEdges vertices 

      --initialGraph =
      --   Graph vertices edges

      initialGraph =
         makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)

      whiteVertices = 
         List.map (\v ->
            {v | color = (Color.rgb 1 1 1)})
            initialGraph.vertices


      createEdge =
         Graph.updateEdge whiteVertices
      newGraph = Graph whiteVertices (List.map createEdge initialGraph.edges)
   in
      ColorDisplay newGraph (Color.rgb 1 1 1) (Color.rgb 1 1 1)

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

colorPalleteTwo : ColorDisplay -> List (S.Svg Msg)
colorPalleteTwo display=
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
   [squareRed, squareGreen]

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

paneThree displaySeries =
   let 
      display =
         case displaySeries.state of
            TwoColor ->
               displaySeries.colorDisplayA
            ThreeColor ->
               displaySeries.colorDisplayB

      colorPalleteHere =
         case displaySeries.state of
            TwoColor ->
               colorPalleteTwo
            ThreeColor ->
               colorPallete
   in
   Graph.displaySvg ((drawGraphForColoring display.graphA) ++ (colorPalleteHere display))

explanationColoring : ColorDisplaySeries -> Bool -> Int -> ELE.Element Msg
explanationColoring colorDispSer helpStatus width =
    let
      colorDisp =
         case colorDispSer.state of
            TwoColor ->
               colorDispSer.colorDisplayA
            ThreeColor ->
               colorDispSer.colorDisplayB
            
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
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.width (ELE.fill |> ELE.maximum (width//2))
         ]
         <|
         [  ELE.el
               [Font.size (getFontSize Head width), Font.heavy]
               (ELE.text "Graph Coloring")
         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text coloringExplanation ]

         ,  ELE.paragraph
               --[ ELE.spacing 8 ] 
               []
               --[ ELE.text howToColor ]
               howToColor
         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     As a challenge you may try to color
                     the graph with only two colors and see if
                     it is feasible.
                     """
               ]

        --, unColorButton
        , colorButtons

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
               <| if List.length coloredVertices > 1 && List.length miscoloredEdges == 0
                    then
                       [ emph CuteGreen "Good going! "
                       , ELE.text "Adjacent Vertices are colored differently."
                       ]
                    else
                      [ ELE.none ]

        , ELE.paragraph
               []
               <| if List.isEmpty miscoloredEdges
                     then
                        [ ELE.none ]
                     else
                        List.concat <| List.map miscolorText miscoloredEdges

        , ELE.paragraph
               []
               <| if List.isEmpty miscoloredEdges
                                then
                                    [ ELE.none ]
                                else
                                   [ emph CuteBlue "Try "

                                   , ELE.text "another color combination. "

                                   , emph CuteGreen "Remember the rule;"

                                   , emph Pink
                                       """
                                       No two adjacent
                                       """
                                   , ELE.text
                                       """
                                       vertices must have the same color!
                                       """
                                   ]

        , ELE.paragraph
               []
               <| if (List.isEmpty miscoloredEdges
                                 && List.length coloredVertices 
                                    == List.length colorDisp.graphA.vertices)
                                then
                                   makeCongrats ++
                                   [ ELE.text
                                       """
                                        Graph has been colored
                                       """
                                   , emph CuteGreen
                                       """
                                       fully 
                                       """
                                   , ELE.text
                                       """
                                       and 
                                       """
                                   , emph CuteGreen
                                       """
                                       correctly.
                                       """
                                   , ELE.text
                                       """
                                       i.e. No two adjacent vertices have the same color.
                                       """
                                   ]
                                else
                                   [ ELE.none ]

         , (if helpStatus == True then (helpParagraph GraphColoringHelp) else ELE.none)
         , lowerNavigation "Max Cut" "Vertex Cover"
         ]


colorButtons : ELE.Element Msg
colorButtons =
   ELE.row
      [ELE.spacing 90, ELE.paddingXY 300 40]
      [  unColorButton
      ,  nextTask
      ]
miscolorText : Graph.Edge -> List (ELE.Element msg)
miscolorText e =
   [ ELE.text "Vertex " 
   , emph Pink (String.fromInt e.vertexOne.name) 
   , ELE.text " and vertex "
   , emph Pink (String.fromInt e.vertexTwo.name)
   , ELE.text " which are "
   , emph Pink "adjacent "
   , ELE.text "to each other are colored with the " 
   , emph Pink "same color. "
   ]

goColorSeries : ColorDisplaySeries -> Msg -> ColorDisplaySeries 
goColorSeries displaySeries msg =
   let 
      newState =
         case msg of
            NextAnimation ->
               case displaySeries.state of
                  TwoColor ->
                     ThreeColor
                  ThreeColor ->
                     TwoColor
            _ ->
               displaySeries.state

      newDisplayA =
         case newState of
            TwoColor ->
               goColor displaySeries.colorDisplayA msg
            ThreeColor ->
               displaySeries.colorDisplayA

      newDisplayB =
         case newState of
            ThreeColor ->
               goColor displaySeries.colorDisplayB msg
            TwoColor ->
               displaySeries.colorDisplayB
    in
    { colorDisplayA = newDisplayA
    , colorDisplayB = newDisplayB
    , state = newState
    }

            

goColor : ColorDisplay -> Msg -> ColorDisplay
goColor display msg =
   case msg of
      ColoringSelectColor color ->
               {display |
                  chosenColor = color
               }
      VertexClicked name ->
         let
            newGraph = Graph.changeColorOfVertex name display.chosenColor display.graphA 
         in
            {display |
                 graphA = newGraph }

      VertexNonColor ->
         let
            whiteVertices = 
               List.map (\v ->
                  {v | color = (Color.rgb 1 1 1)})
                  display.graphA.vertices

            createEdge =
               Graph.updateEdge whiteVertices

            newGraph = Graph whiteVertices (List.map createEdge display.graphA.edges)
         in

         {display |
              graphA = newGraph }

      _ ->
         display


drawGraphForColoring g =
    let
      verticesOfSameColor edge =
         edge.vertexOne.color == edge.vertexTwo.color && edge.vertexOne.color /= (Color.rgb 1 1 1)

      normalEdges = List.filter (\e -> not (verticesOfSameColor e)) g.edges 

      miscoloredEdges = List.filter (\e -> verticesOfSameColor e) g.edges 
    in
    List.map Graph.drawEdge normalEdges
        ++ List.map Graph.drawSpecialEdge miscoloredEdges
        ++ List.map Graph.drawVertex g.vertices
        ++ List.map Graph.writeVertexName g.vertices
