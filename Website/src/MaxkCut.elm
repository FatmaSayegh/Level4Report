module MaxkCut exposing (..)

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
import FontSize exposing (getFontSize, FontSize(..))

type alias MaxCutTransition =
    { transitionA : ShapeTransition -- Will remain static
    , transitionB : ShapeTransition  -- Will move towards final Grid when animationOn is True
    , state : MaxCutState
    }

type MaxCutState =
   TwoCut
   | ThreeCut

-- Initializing the model
-- init function initializes, a model and provides
-- an instance of the model to the elm runtime.

toggleToken : Token -> Token
toggleToken token =
   case token of
      MakeKCut ->
         NoToken
      NoToken ->
         MakeKCut


maxcutTransitionA : ShapeTransition
maxcutTransitionA =
    let
        (initialGraph, finalGrid) =
            maxCutGeometry
    in
        { graphA = initialGraph
        , graphB = initialGraph
        , finalGrid = finalGrid
        , animationOn = False
        , specialToken = NoToken
        , time = 0.0
        }

maxcutTransitionB : ShapeTransition
maxcutTransitionB =
    let
        (initialGraph, finalGrid) =
            threeCutGeometry
    in
        { graphA = initialGraph
        , graphB = initialGraph
        , finalGrid = finalGrid
        , animationOn = False
        , specialToken = NoToken
        , time = 0.0
        }

maxCutTransition : MaxCutTransition
maxCutTransition =
   { transitionA = maxcutTransitionA
   , transitionB = maxcutTransitionB
   , state = TwoCut
   }

maxCutGeometry : (Graph, Grid)
maxCutGeometry =
   let
      position = (vec3 100 200 0) -- left center
      verticalShift = (vec3 0 50 0)
      verticalShiftGrid = (vec3 0 90 0)
      horizontalShiftGrid = (vec3 200 0 0)
      setA = [1,2,3,4]
      setB = [5,6,7,8]
      --edgeTuples = [ (1, 8), (1, 7), (1, 6), (2, 5), (2, 6), (2, 7), (3, 7), (3, 8), (3, 6), (3, 4), (4, 5)]
      edgeTuples = [ (1, 8), (1, 7), (2, 6), (2, 7), (3, 5), (3, 7), (3, 8), (3, 6), (3, 4), (4, 5)]
      setAGrid = parametricPolygon 4 (vec3 50 30 0) (sub position  verticalShift) (pi/3)
      setBGrid = parametricPolygon 4 (vec3 50 30 0) (add position verticalShift) (pi/6)
      setAGridPosition = (add (sub (sub position verticalShift) verticalShiftGrid) horizontalShiftGrid)
      setBGridPosition = (add (add (add position verticalShift) verticalShiftGrid) horizontalShiftGrid)
      setAFinalGrid = parametricPolygon 4 (vec3 70 10 0) setAGridPosition (pi/3)
      setBFinalGrid = parametricPolygon 4 (vec3 70 10 0) setBGridPosition (pi/3)
      vertices = 
         List.map3 (\name g c -> Graph.Vertex name g c False) 
            (setA ++ setB)
            (setAGrid ++ setBGrid)
            (Graph.listOfColors First 8)
      edges =
         Graph.makeEdgesWithTuples edgeTuples vertices

      in
      (Graph vertices edges, setAFinalGrid ++ setBFinalGrid)

threeCutGeometry : (Graph, Grid)
threeCutGeometry =
   let
      position = (vec3 200 180 0) -- left center
      distance =
         100
      angle = pi/6
      verticalShift = vec3 0 (distance * sin angle) 0
      longVerticalShift = vec3 0 distance 0
      horizontalShift = vec3 (distance * cos angle) 0 0
      edgeTuples 
         = [  (1, 4), (1, 5), (1, 6), (1, 7), (1, 8), (1, 9)
           ,  (2, 4), (2, 5), (2, 6), (2, 7), (2, 8), (2, 9)
           ,  (3, 4), (3, 5), (3, 6), (3, 7), (3, 8), (3, 9)
           ,  (4, 7), (4, 8), (4, 9)
           ,  (5, 7), (5, 8), (5, 9)
           ,  (6, 7), (6, 8), (6, 9)
           ]
      gridStart = parametricPolygon 9 (vec3 80 80 0) position (pi/2 - 2*pi/9)
      modifiedGrid =
         gridStart
            |> List.map2 (\x y -> (x, y)) (List.range 1 9)
            |> List.map (\t ->
                           if Tuple.first t <= 3
                              then
                                 (add (Tuple.second t) longVerticalShift)
                              else
                                 if Tuple.first t > 3 && Tuple.first t <= 6
                                    then
                                    (sub (sub (Tuple.second t) horizontalShift) verticalShift)
                                    else
                                    (sub (add (Tuple.second t) horizontalShift) verticalShift)
                        )

      vertices = 
         List.map3 (\name g c -> Graph.Vertex name g c False) 
            (List.range 1 9)
            (gridStart)
            (Graph.listOfColors First 9)
      edges =
         Graph.makeEdgesWithTuples edgeTuples vertices

      in
      (Graph vertices edges, modifiedGrid)

explanationTwo : MaxCutTransition -> Bool -> Int -> ELE.Element Msg
explanationTwo maxCut helpStatus width =
      let 
         shapeTransition =
            case maxCut.state of
               TwoCut ->
                  maxCut.transitionA
               ThreeCut ->
                  maxCut.transitionB
         state =
            maxCut.state

         twoCutExplanation =
            """
            In the animation, the vertices are being segregated into two sets,
            such that the number of edges passing from vertices in one set to
            the vertices in another set is more than any other way the vertices
            of the graph could have been segregated.  In other words the
            problem of max cut is to identify such partition of the vertices of
            the graph that the above objective is satisfied.
            """

         threeCutExplanation =
            """
            In the animation, the vertices are being segregated into three
            sets, such that the number of edges passing from vertices in one
            set to the vertices in all other sets is more than any other way
            the vertices of the graph could have been segregated.  In other
            words the problem of max 3 cut is to identify such partition of the
            vertices of the graph that the above objective is satisfied.
            """

         twoCutLineExplanation =
            """
            The Max cut line, seperates the two sets of vertices. The
            intersection between the cut line and the edges are shown as blue
            dots. As you should verify, they are 9 in number. This number is
            equal to number of edges from the set of vertices at the top going
            to the vertices at the bottom.
            """

         threeCutLineExplanation =
            """
            The three Max cut lines, seperates their respective sets from the
            rest of the graph. The intersection between the cut lines and the
            edges are shown as blue dots. As you should verify, they are 18 in
            number for each set. This 3 cut is visually trivial as the graph
            was tripartite.
            """

      in
      ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.height ELE.fill
         , ELE.spacing 20
         --, ELE.padding 40
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.width (ELE.fill |> ELE.maximum (width//2))
         ]
         <|
         [  ELE.el
               [Font.size (getFontSize Head width), Font.heavy] 
               (ELE.text "Max Cut")
         ,  ELE.paragraph
               [ELE.spacing 8] 
               [ELE.text maxCutExplanation]
               --TODO

         ,  mediaButtonsForMaxCut shapeTransition

         , ELE.paragraph
               []
               [ ELE.text 
                     <| if state == TwoCut
                           then
                              twoCutExplanation
                           else
                              threeCutExplanation
               ]
           
        , buttonWrap "Cutline" <| Input.button
                                  [
                                    ELE.centerX
                                  ] 
                                  { onPress = Just MaxCutLine
                                  , label = Icons.minusOutlined [ Ant.width 70, Ant.height 50 ]
                                  }

        ,  if helpStatus == False
               then
                  ELE.paragraph
                     []
                     [ELE.text <| if shapeTransition.specialToken == MakeKCut 
                                    then
                                       if state == TwoCut
                                          then
                                            twoCutLineExplanation    
                                          else 
                                            threeCutLineExplanation    
                                    else
                                       ""
                     ]
               else
                  helpParagraph MaxCutHelp

          , lowerNavigation "Isomporphism" "Graph Coloring"
          ]

buttonWrap : String -> (ELE.Element Msg) -> ELE.Element Msg
buttonWrap description button =
   let
      descriptionStyle =
         [ Font.color (ELE.rgb 0.5 0.5 0.5)
         , Font.size 10
         , ELE.centerX
         ]
   in
   ELE.column
   [ ELE.centerX
   ]
   [ button
   , ELE.el descriptionStyle (ELE.text description)
   ]

paneTwo maxCutTrans =
   case maxCutTrans.state of
      TwoCut ->
         paneTwoA maxCutTrans.transitionA
      ThreeCut ->
         paneTwoB maxCutTrans.transitionB

paneTwoA shapeTransition =
   let
      graphA = shapeTransition.graphA
      graphB = shapeTransition.graphB
   in
   case shapeTransition.specialToken of
      MakeKCut ->
         let
            cutLine = makeCutLine shapeTransition
         in
         Graph.displaySvg (   (Graph.drawGraph graphA) 
                           ++ (Graph.drawGraph graphB) 
                           ++ (drawCutLine cutLine)  )

      NoToken ->
         Graph.displaySvg ((Graph.drawGraph graphA) ++ (Graph.drawGraph graphB))

paneTwoB shapeTransition =
   let
      graphA = shapeTransition.graphA
      graphB = shapeTransition.graphB
   in
   case shapeTransition.specialToken of
      MakeKCut ->
         let
            cutLines = 
               makeCutLineB shapeTransition
            drawCutLines =
               cutLines
               |> List.map drawCutLine
               |> List.concat
         in
         Graph.displaySvg ((Graph.drawGraph graphB) ++ drawCutLines)

      NoToken ->
         Graph.displaySvg (Graph.drawGraph graphB)

animateMaxCutCompound : Msg -> MaxCutTransition -> MaxCutTransition
animateMaxCutCompound  msg maxCutTrans =
   case msg of
      NextAnimation ->
         let
            newState =
               case maxCutTrans.state of
                  TwoCut ->
                     ThreeCut
                  ThreeCut ->
                     TwoCut
         in
         { maxCutTrans
            | state = newState
         }

      _ ->

        case maxCutTrans.state of
           TwoCut ->
              let 
                  shapeTrans =
                     animateMaxCutTransition msg maxCutTrans.transitionA
              in
              {  maxCutTrans
                 | transitionA = shapeTrans
              }

           ThreeCut ->
              let 
                  shapeTrans =
                     animateMaxCutTransition msg maxCutTrans.transitionB
              in
              {  maxCutTrans
                 | transitionB = shapeTrans
              }

animateMaxCutTransition : Msg -> ShapeTransition -> ShapeTransition
animateMaxCutTransition  msg shapeTransition =
   case msg of
       TimeDelta delta ->
           case shapeTransition.animationOn of
               True ->
                   Graph.executeShapeTransition delta shapeTransition
               False ->
                   shapeTransition

       AnimationToggle ->
           { shapeTransition
               | animationOn = not shapeTransition.animationOn
           }

       AnimationStartOver ->
           { shapeTransition
               | graphB = shapeTransition.graphA
               , time = 0.0
           }

       MaxCutLine ->
           { shapeTransition
               | specialToken = toggleToken shapeTransition.specialToken
           }


       _ ->
           shapeTransition

makeCutLineB shapeTransition = 
   let
      vertices =
         shapeTransition.graphB.vertices

      firstTuple = [ (4,6), (7,9), (1,3)]

      listOfTupledPosns =
         firstTuple
            |> List.filterMap (Graph.findPositionsOfTuples vertices)

      edgeLines =
         Graph.findEdgeLines shapeTransition.graphB.edges

      intersectionPoints =
         listOfTupledPosns
            |> List.map (\(p1, p2) ->
                           List.filterMap (Graph.findIntersection (p1, p2)) edgeLines)
   in
   List.map2 
         (\(p1, p2) ins ->
            CutLine p1 p2 ins)
         listOfTupledPosns 
         intersectionPoints

makeCutLine shapeTransition =
   let
      start
         = vec3 210 155 0
      end
         = vec3 370 155 0

      setA = [1,2,3,4]

      setB = [5,6,7,8]

      edgeLines =
         Graph.findEdgeLines shapeTransition.graphB.edges

      intersectionPoints = 
         List.filterMap (Graph.findIntersection (start, end)) edgeLines

   in
   CutLine start end (intersectionPoints)

type CutLine =
   CutLine Vec3 Vec3 (List Vec3)

drawCutLine cutLine =
    case cutLine of
    CutLine start end l ->
      [(Graph.line start end)] ++ (Graph.drawIntersectionPoints l)

mediaButtonsForMaxCut : ShapeTransition -> ELE.Element Msg
mediaButtonsForMaxCut shapeTransition =
   ELE.row
      [ELE.spacing 90, ELE.paddingXY 300 40]
      [  playButton shapeTransition.animationOn
      ,  resetButton
      ,  forwardButton
      ]
