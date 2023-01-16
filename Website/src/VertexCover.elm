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
import FontSize exposing
               ( getFontSize
               , FontSize(..)
               , FontColor(..)
               , giveFontColor
               , emphForScreen
               , DisplaySize
               , DeviceType(..)
               )

type alias VertexCoverDisplay =
   { graphA : Graph
   , graphB : Graph
   , state : VertexCoverState
   }

type VertexCoverState =
   First
   | Second

vertexCoverDisplay : VertexCoverDisplay     
vertexCoverDisplay = 
   let
        graphA =
            makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)

        graphB =
            makeGraph (PolygonCycleDoll 6) (vec3 200 100 0) (vec3 80 80 0) (2 * pi / 3)
   in
      VertexCoverDisplay graphA graphB First

paneFour display =
   case display.state of
      First ->
         Graph.displaySvg (drawGraphForCover display.graphA)
      Second ->
         Graph.displaySvg (drawGraphForCover display.graphB)

goCover : VertexCoverDisplay -> Msg -> VertexCoverDisplay
goCover display msg =
   case msg of
       ToggleVertexStatus name ->
            case display.state of
               First ->
                  { display
                      | graphA = Graph.toggleGlowVertex name display.graphA
                  }

               Second ->
                  { display
                      | graphB = Graph.toggleGlowVertex name display.graphB
                  }

       VertexClicked name ->
            case display.state of
               First ->
                  { display
                      | graphA = Graph.toggleGlowVertex name display.graphA
                  }

               Second ->
                  { display
                      | graphB = Graph.toggleGlowVertex name display.graphB
                  }

       NextAnimation ->
            case display.state of
               First ->
                  { display
                     | state = Second
                  }

               Second ->
                  { display
                     | state = First
                  }

       _ ->
            display

explanationCover : VertexCoverDisplay -> Bool -> DisplaySize -> ELE.Element Msg
explanationCover display helpStatus displaySize =
    let
        emph =
            emphForScreen displaySize.deviceType

        graph =
            case display.state of
               First ->
                  display.graphA
               Second ->
                  display.graphB

        selected_vertices =
            List.filter (\ver -> ver.glow) graph.vertices

        noOfSelectedVertices =
            List.length selected_vertices

        ( coveredEdges, _ ) =
            Graph.seperateEdges graph

        noCoveredEdges =
            List.length coveredEdges


        totalEdges =
            List.length graph.edges

        totalVertices =
            List.length graph.vertices

        edgesRemainig = 
            totalEdges - noCoveredEdges
        
    in
    ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.spacing 20
         , ELE.height ELE.fill
         , ELE.width (ELE.fill |> ELE.maximum displaySize.width)
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.scrollbarY
         ]
         <|
         [  ELE.el
               [Font.size (getFontSize Head displaySize.deviceType), Font.heavy] 
               (ELE.text "Vertex Cover")

         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text vertexCoverExplanation ]

         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     In the task on the right, selecting a vertex will cover all
                     the edges
                     """
               , emph CuteGreen
                     """
                     incident 
                     """
               , ELE.text
                     """
                     on it. Your objective is to select the
                     """
               , emph CuteGreen
                     """
                     minimum 
                     """
               , ELE.text
                     """
                     number of vertices such that, all the edges of the
                     graph are covered.
                     """
               ]

         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     To select a vertex you can 
                     """
               , emph CuteBlue
                     """
                      press 
                     """
               , ELE.text  
                     """
                     , the vertex 
                     """
               , emph CuteBlue
                     """
                      number
                     """
               , ELE.text
                     """
                     on the keyboard. To 
                     """
               , emph CuteBlue
                     """
                     de-select 
                     """
               , ELE.text
                     """
                     , press the same key again.
                     """
               ]

        , ELE.paragraph
               []
               <| if noOfSelectedVertices  == 0
                                then
                                   [ ELE.none ]
                                else
                                   [ ELE.text "You have selected a total of "
                                   , emph Pink (String.fromInt noOfSelectedVertices)
                                   , ELE.text " vertices "
                                   , emph CuteGreen "out of "
                                   , emph Pink (String.fromInt totalVertices)
                                   , ELE.text " vertices. "
                                   ]

        , ELE.paragraph
               []
               <| if noCoveredEdges == 0
                     then
                        [ ELE.none ]
                     else
                        [ ELE.text "You have covered a total of "
                        , emph Pink (String.fromInt noCoveredEdges)
                        , ELE.text " edges " 
                        , emph CuteGreen "out of " 
                        , ELE.text "a total of "
                        , emph Pink (String.fromInt totalEdges)
                        , ELE.text  " edges. "
                        ]

        , ELE.paragraph
               []
               <| if edgesRemainig == 0
                      then
                         if noOfSelectedVertices > 4
                            then
                               [ ELE.text
                                  """
                                  You have covered all the edges.
                                  but
                                  you have done so by selecting
                                  """
                               , emph Pink (String.fromInt noOfSelectedVertices)
                               , ELE.text 
                                  """
                                  vertices. The graph could have been covered by
                                  selecting only four! Try again to see that
                                  you can do it in just four.
                                  """
                               ]
                            else
                               
                                  (makeCongrats displaySize.deviceType) ++
                               [  ELE.text " you have covered all "
                               ,  emph CuteBlue (String.fromInt noCoveredEdges)
                               ,  ELE.text " edges. "
                               ,  ELE.text "You have done so by selecting the vertices "
                               ,  emph CuteBlue (Graph.getStringFromVertices selected_vertices)
                               ,  ELE.text ". Therefore a vertex cover of this graph is the"
                               ,  emph CuteGreen " set " 
                               ,  ELE.text " of vertices "
                               ,  emph CuteBlue (Graph.getStringFromVertices selected_vertices)
                               ,  ELE.text "."
                               ]

                      else
                         if edgesRemainig == totalEdges
                         then
                            [ ELE.none ]
                         else
                            [ emph Pink (String.fromInt edgesRemainig)
                            , ELE.text  " edges more to be covered!"
                            ]

         ,  (if helpStatus == True then (helpParagraph VertexCoverHelp) else ELE.none)
         , vertexButtons
          , lowerNavigation "Graph Coloring" "Tree Width"
       ]
vertexButtons =
   ELE.row
      [ELE.centerX]
      [ tryDifferent ]

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
        ++ List.map Graph.drawVertex g.vertices
        ++ List.map Graph.drawSelectedVertex selectedVertices
        ++ List.map Graph.writeVertexName g.vertices
