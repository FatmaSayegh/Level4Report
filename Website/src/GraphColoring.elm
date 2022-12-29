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

explanationColoring : ColorDisplay -> Bool -> ELE.Element Msg
explanationColoring colorDisp helpStatus =
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
         , ELE.width ELE.fill
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Graph Coloring")
         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text coloringExplanation ]

         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text howToColor ]
         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     As a challenge you may try to color
                     the graph with only two colors and see if
                     it is feasible.
                     """
               ]
        , Input.button
            [
              ELE.centerX
            ] 
            { onPress = Just VertexNonColor
            , label = Icons.rollbackOutlined [ Ant.width 70, Ant.height 50 ]
            }

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
               [ ELE.text <| if List.length coloredVertices > 1 && List.length miscoloredEdges == 0
                                then
                                   "Good going! Adjacent Vertices are colored differently."
                                else
                                   """
                                   """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if List.isEmpty miscoloredEdges
                                then
                                    ""
                                else
                                    String.join " "  <| List.map miscolorText miscoloredEdges
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if List.isEmpty miscoloredEdges
                                then
                                    ""
                                else
                                   """
                                   Try another color combination.
                                   Remember the rule; No two adjacent
                                   vertices must have the same color!
                                   """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if (List.isEmpty miscoloredEdges
                                 && List.length coloredVertices 
                                    == List.length colorDisp.graphA.vertices)
                                then
                                   """
                                   Congratulations! Graph has been colored fully and correctly.
                                   i.e. No two adjacent vertices have the same color.
                                   """
                                else
                                   ""
              ]
         , (if helpStatus == True then (helpParagraph GraphColoringHelp) else ELE.none)
         , lowerNavigation "Max Cut" "Vertex Cover"
         ]



miscolorText : Graph.Edge -> String
miscolorText e =
   "Vertex " ++ (String.fromInt e.vertexOne.name) 
             ++ " and vertex "
             ++ (String.fromInt e.vertexTwo.name)
             ++ " which are adjacent to each other are colored with the same color."

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
