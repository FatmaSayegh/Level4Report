module Isomorphism exposing (..)

import Graph exposing (Graph, Grid, makeGraph, GType(..), ShapeTransition, Token(..))
import Math.Vector3 exposing (..)
import Messages exposing (Msg(..))
import Element as ELE
import Element.Background as Background
import Element.Font as Font



isomorphicTransition : ShapeTransition
isomorphicTransition =
    let
        initialGraph =
            makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)
    in
        { graphA = initialGraph
        , graphB = initialGraph
        , finalGrid = bipartiteGrid
        , animationOn = False
        , specialToken = NoToken
        }

animateIsomorphicTransition : Msg -> ShapeTransition -> ShapeTransition
animateIsomorphicTransition msg shapeTransition =
   case msg of
       TimeDelta delta ->
           case shapeTransition.animationOn of
               True ->
                   executeShapeTransition shapeTransition
               False ->
                   shapeTransition
   
       HoverOver name ->
           { shapeTransition
               | graphA = changeGlowVertex True name <| makeUnglowAllVertices shapeTransition.graphA
               , graphB = changeGlowVertex True name <| makeUnglowAllVertices shapeTransition.graphB
           }
   
       MouseOut name ->
           { shapeTransition
             | graphA = changeGlowVertex False name shapeTransition.graphA
             , graphB = changeGlowVertex False name shapeTransition.graphB
           }
   
       ToggleVertexStatus name ->
           { shapeTransition
               | graphA = toggleGlowVertex name <| makeUnglowAllVerticesBut name shapeTransition.graphA
               , graphB = toggleGlowVertex name <| makeUnglowAllVerticesBut name shapeTransition.graphB
           }
   
       AnimationToggle ->
           { shapeTransition
               | animationOn = not shapeTransition.animationOn
           }
   
       AnimationStartOver ->
           { shapeTransition
               | graphB = shapeTransition.graphA
           }

       Other ->
           shapeTransition

       NextTopic ->
           shapeTransition
       _ ->
           shapeTransition

makeStory : ShapeTransition -> Bool -> List (ELE.Element Msg)
makeStory shapeTransition helpStatus =
    let
        glowing_vertices =
            List.filter (\ver -> ver.glow) shapeTransition.graphB.vertices


        putyourmouse =
            """
            Go ahead and put your mouse over a vertex of the graph.
            Or press a number on the keyboard corresponding to a Vertex number.
            """

        ( specialEdges, _ ) =
            seperateEdges shapeTransition.graphB

        relatedVertices =
            getHaloVertices shapeTransition.graphB specialEdges

        connectedToThis v =
            "And connected to vertex {{ }} are the vertices " |> String.Format.value (String.fromInt <| v.name)

        whichYouCanSee =
            " Which you can see is true for both graphs."

        listOfStories =
            case glowing_vertices of
                [] ->
                    []

                x :: xs ->
                    [ "You have selected Vertex {{}}." |> String.Format.value (String.fromInt x.name)
                    , (connectedToThis x)
                        ++ getStringFromVertices relatedVertices
                    , whichYouCanSee
                    ]

        storyPara =
            List.intersperse (ELE.html <| H.br [] [])
                (List.map ELE.text
                    listOfStories
                )

        footer =
            case glowing_vertices of
               [] -> ""
               x :: xs -> 
                  """
                  You may want to visit other vertices to see that, each vertex
                  is connected to the same vertices in both graphs.
                  Inspecting each vertices connectivity with other vertices, in both graphs you can 
                  convince your self that the graphs are isomorphic to each other.
                  """
    in
    if helpStatus == False
      then
         [ ELE.paragraph [] storyPara
         , ELE.paragraph [] [ELE.text footer]
         ]
      else
         [ helpParagraph IsomorphismHelp]

drawGraph g =
    let
        ( specialEdges, normalEdges ) =
            seperateEdges g

        haloVertices = getHaloVertices g specialEdges

        selectedVertices =
            List.filter (\ver -> ver.glow) g.vertices
         
    in
    List.map drawEdge normalEdges
        ++ List.map drawSpecialEdge specialEdges
        ++ List.map drawGoldenCircle haloVertices
        ++ List.map drawVertex g.vertices
        ++ List.map drawSelectedVertex selectedVertices
        ++ List.map writeVertexName g.vertices

paneOne graphA graphB =
    displaySvg ((drawGraph graphA) ++ (drawGraph graphB))

linearGridLeft =
    linearGrid 4 (vec3 150 250 0) (vec3 0 120 0)


linearGridRight =
    linearGrid 4 (vec3 250 250 0) (vec3 0 120 0)



-- So that our project does not become graph problem solving
-- We have the answer to the isomorphism problem here


setLeft : List Int
setLeft =
    [ 3, 8, 6, 1 ]

setInner : List Int
setInner =
    [3, 1,6,8 ]

setRight : List Int
setRight =
    [ 7, 4, 2, 5 ]

setOuter : List Int
setOuter =
    [7,4,2,5]


-- Here as set of numbers in the left and the right are being tupled with list of vertical vector grids
-- and then sorted according to index numbers
-- This gives node 1 its vector at position left of bipartite graph
-- This gives node 6 its vector at position left of bipartite graph
-- What it does is that the vector on the second position on the left grid goes to 6th on the final grid
-- Vector on the 3rd position of the left grid goes to the 8th on the final grid


bipartiteGrid =
    let
        leftTupled =
            List.map2 (\x y -> ( x, y )) setLeft linearGridLeft

        rightTupled =
            List.map2 (\x y -> ( x, y )) setRight linearGridRight

        totalGrid =
            leftTupled ++ rightTupled
    in
    List.map (\( x, y ) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) totalGrid)

innerPolygon = parametricPolygon 4 (vec3 40 40 0) (vec3 200 300 0) (pi/2)
outerPolygon = parametricPolygon 4 (vec3 80 80 0) (vec3 200 300 0) 0

starGrid =
   let
      positionsTupled =
         List.map2 (\x y -> (x,y)) (setInner ++ setOuter) (innerPolygon ++ outerPolygon )
   in
   List.map (\(x,y) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) positionsTupled)

explanationOne : ShapeTransition -> Bool -> ELE.Element Msg
explanationOne shapeTransition helpStatus =
      ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.height ELE.fill
         , ELE.spacing 20
         --, ELE.padding 40
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.height ELE.fill
         , ELE.width ELE.fill
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Graph Isomorphism")
         ,  ELE.paragraph
               [ELE.spacing 8] 
               [ELE.text isomorphismExplanation]

         , mediaButtons shapeTransition


         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     Go ahead and put your mouse over a vertex of the graph. 
                     Or press a number on the keyboard corresponding to a Vertex number.
                     """
               ]

         ]

         ++  
            (makeStory shapeTransition helpStatus)
         ++


         [ lowerNavigation "Tree Width" "Max Cut" ]
