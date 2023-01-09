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

colorDisplay : ColorDisplay
colorDisplay = 
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

paneThree display =
   Graph.displaySvg ((drawGraphForColoring display.graphA) ++ (colorPallete display))

explanationColoring : ColorDisplay -> Bool -> Int -> ELE.Element Msg
explanationColoring colorDisp helpStatus width =
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

        , unColorButton

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

makeCongrats =
   [ emph CuteGreen "C"
   , emph CuteBlue "o"
   , emph Pink "n"
   , emph CuteGreen "g"
   , emph CuteBlue "r"
   , emph Pink "a"
   , emph CuteGreen "t"
   , emph CuteBlue "u"
   , emph Pink "l"
   , emph CuteGreen "a"
   , emph CuteBlue "t"
   , emph Pink "i"
   , emph CuteGreen "o"
   , emph CuteBlue "n"
   , emph Pink "s"
   , emph CuteGreen "!"
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
