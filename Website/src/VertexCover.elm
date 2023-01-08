module VertexCover exposing (..)

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
import FontSize exposing (getFontSize, FontSize(..))

type alias VertexCoverDisplay =
   { graphA : Graph
   }

vertexCoverDisplay : VertexCoverDisplay     
vertexCoverDisplay = 
   let
        initialGraph =
            makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)
   in
      VertexCoverDisplay initialGraph

paneFour display =
         Graph.displaySvg (drawGraphForCover display.graphA)

goCover : VertexCoverDisplay -> Msg -> VertexCoverDisplay
goCover display msg =
   case msg of
       ToggleVertexStatus name ->
            { display
                | graphA = Graph.toggleGlowVertex name display.graphA
            }

       VertexClicked name ->
            { display
                | graphA = Graph.toggleGlowVertex name display.graphA
            }

       _ ->
            display

explanationCover : VertexCoverDisplay -> Bool -> Int -> ELE.Element Msg
explanationCover display helpStatus width =
    let
        selected_vertices =
            List.filter (\ver -> ver.glow) display.graphA.vertices

        noOfSelectedVertices =
            List.length selected_vertices

        ( coveredEdges, _ ) =
            Graph.seperateEdges display.graphA

        noCoveredEdges =
            List.length coveredEdges


        totalEdges =
            List.length display.graphA.edges

        totalVertices =
            List.length display.graphA.vertices

        edgesRemainig = 
            totalEdges - noCoveredEdges
        
    in
    ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.spacing 20
         , ELE.height ELE.fill
         , ELE.width (ELE.fill |> ELE.maximum (width//2))
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         ]
         <|
         [  ELE.el
               [Font.size (getFontSize Head width), Font.heavy] 
               (ELE.text "Vertex Cover")

         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text vertexCoverExplanation ]

         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     In the task on the right, selecting a vertex will cover all
                     the edges incident on it. Your objective is to select the
                     minimum number of vertices such that, all the edges of the
                     graph are covered.
                     """
               ]

         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     To select a vertex you can press, the vertex number
                     on the keyboard. To de-select, do the same again.
                     """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if noOfSelectedVertices  == 0
                                then
                                   ""
                                else
                                   "You have selected a total of "
                                   ++ (String.fromInt noOfSelectedVertices)
                                   ++ " vertices out of "
                                   ++ (String.fromInt totalVertices)
                                   ++ " vertices. "
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if noCoveredEdges == 0
                     then
                        ""
                     else
                        "You have covered a total of "
                        ++ (String.fromInt noCoveredEdges)
                        ++ " edges out of a total of "
                        ++ (String.fromInt totalEdges)
                        ++ " edges. "
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if edgesRemainig == 0
                              then
                                 if noOfSelectedVertices > 4
                                    then
                                       """
                                       You have covered all the edges.
                                       but
                                       you have done so by selecting
                                       """
                                       ++
                                       (String.fromInt noOfSelectedVertices)
                                       ++
                                       """
                                       vertices. The graph could have been covered by
                                       selecting only four! Try again to see that
                                       you can do it in just four.
                                       """
                                    else
                                       "Congratulations, you have covered all "
                                       ++ (String.fromInt noCoveredEdges)
                                       ++ " edges. "
                                       ++ "You have done so by selecting the vertices "
                                       ++ Graph.getStringFromVertices selected_vertices
                                       ++ "."
                                       ++ " Therefore a vertex cover of this graph is the set vertices "
                                       ++ Graph.getStringFromVertices selected_vertices
                                       ++ "."

                              else
                                 if edgesRemainig == totalEdges
                                 then
                                    ""
                                 else
                                    (String.fromInt edgesRemainig)
                                    ++ " edges more to be covered!"
              ]

         ,  (if helpStatus == True then (helpParagraph VertexCoverHelp) else ELE.none)
          , lowerNavigation "Graph Coloring" "Tree Width"
       ]

drawGraphForCover g =
    let
        ( specialEdges, normalEdges ) =
            Graph.seperateEdges g

        haloVertices =
            Graph.getHaloVertices g specialEdges

        selectedVertices =
            List.filter (\ver -> ver.glow) g.vertices
         
    in
    List.map Graph.drawEdge normalEdges
        ++ List.map Graph.drawSpecialEdge specialEdges
    --    ++ List.map drawGoldenCircle haloVertices
        ++ List.map Graph.drawVertex g.vertices
        ++ List.map Graph.drawSelectedVertex selectedVertices
        ++ List.map Graph.writeVertexName g.vertices
