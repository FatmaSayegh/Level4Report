module Isomorphism exposing (..)

import Graph exposing (Graph, InPlaceTransition, linearGrid, parametricPolygon, Grid, makeGraph, Gtype(..), ShapeTransition, Token(..))
import Math.Vector3 exposing (..)
import Messages exposing (Msg(..))
import Element as ELE
import Element.Background as Background
import Element.Font as Font
import Explanation exposing (..)
import Buttons exposing (..)
import String.Format
import Html as H exposing (div, h1, p, text)
import Svg as S exposing (..)
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


miniIsoGraph = 
      makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)
      |> drawGraph
      |> Graph.displaySvg

type IsoState =
   Transition
   | Game

type alias IsomorphicTopic =
   { shapeTransition : ShapeTransition
   , isomorphicGame : IsomorphicGame
   , topicState : IsoState
   }

isomorphicTopic : IsomorphicTopic
isomorphicTopic =
   { shapeTransition = isomorphicTransition
   , isomorphicGame = isomorphicGame
   , topicState = Transition
   }
      

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
        , time = 0.0
        }

type alias IsomorphicGame =
   { transition : InPlaceTransition
   , graphB : Graph
   , graphC : Graph
   , choiceState : ChoiceState
   , gameState : GameState
   }

type ChoiceState =   
   NoChoice
   | FirstGraph
   | SecondGraph

type GameState =
   NoCheck
   | Check

inplaceTransition : InPlaceTransition
inplaceTransition =
   let
      graph =
            makeGraph (PolygonCycleDoll 4) (vec3 100 200 0) (vec3 80 80 0) (pi / 4)
   in
   { graph = graph
   , backupGraph = graph
   , grid = bipartiteGridInPlace
   , animationOn = False
   , time = 0.0
   }

removeFirstEdge : Graph -> Graph
removeFirstEdge graph =
   let
      newEdges =
         Maybe.withDefault [] <| List.tail graph.edges
   in
      { graph |
         edges = newEdges }

isomorphicGame : IsomorphicGame
isomorphicGame =
    let
        initialGraph pos =
            makeGraph (PolygonCycleDoll 4) pos (vec3 80 80 0) (pi / 4)

        removedEdgeGraph =
            Graph.morphGraph (initialGraph (vec3 300 300 0)) bipartiteGridInPlaceThird
            |> removeFirstEdge
         
        

    in
    { transition = inplaceTransition
    , graphB = Graph.morphGraph (initialGraph (vec3 300 100 0)) bipartiteGridInPlaceSecond
    , graphC = removedEdgeGraph
    , choiceState = NoChoice
    , gameState = NoCheck
    }

animateIsomorphicTopic : Msg -> IsomorphicTopic -> IsomorphicTopic
animateIsomorphicTopic msg topic =
   case msg of
      NextAnimation ->
         case topic.topicState of
            Transition ->
               {topic | topicState = Game }
            Game ->
               {topic | topicState = Transition}
      _ ->
            
         case topic.topicState of 
            Transition ->
                let
                  shapeTransition =
                        animateIsomorphicTransition msg topic.shapeTransition
                in
                {topic | shapeTransition = shapeTransition } 
            
            Game ->
               let
                  newGame =
                     getNewGame msg topic.isomorphicGame 
               in
               {topic | isomorphicGame = newGame }
            
            
getNewGame : Msg -> IsomorphicGame -> IsomorphicGame
getNewGame msg game =
   case msg of
      IsoChoiceOne ->
         { game | choiceState = FirstGraph 
                , gameState = NoCheck
                , transition = animateIsomorphicGameTrans AnimationStartOver game.transition
                , graphB = Graph.changeGlowVertex False 1 game.graphB
                , graphC = Graph.changeGlowVertex False 1 game.graphC
         }
      IsoChoiceTwo ->
         { game | choiceState = SecondGraph
                , gameState = NoCheck
                , transition = animateIsomorphicGameTrans AnimationStartOver game.transition
                , graphB = Graph.changeGlowVertex False 1 game.graphB
                , graphC = Graph.changeGlowVertex False 1 game.graphC
         }

      HoverOver name ->
           case game.gameState of
               NoCheck ->
                  let
                     transition = game.transition
                  in
                  { game
                      | graphB =
                            game.graphB
                            |> Graph.makeUnglowAllVertices
                            |> Graph.changeGlowVertex True name
                      , graphC = 
                            game.graphC
                            |> Graph.makeUnglowAllVertices
                            |> Graph.changeGlowVertex True name

                      , transition =
                            { transition |
                                 graph = 
                                    transition.graph
                                    |> Graph.makeUnglowAllVertices
                                    |> Graph.changeGlowVertex True name
                             }
                  }

               _ ->
                  game
   
      MouseOut name ->
           case game.gameState of
               NoCheck ->
                  let
                     transition = game.transition
                  in
                  { game
                    | graphC = 
                         game.graphC
                         |> Graph.changeGlowVertex False name

                    , graphB = 
                         game.graphB
                         |> Graph.changeGlowVertex False name

                    , transition =
                          let
                            graph = 
                               transition.graph
                               |> Graph.makeUnglowAllVertices
                          in
                          { transition | graph = graph }
                  }
               _ ->
                  game
   

      IsoCheck ->
         let
            gameState =
               game.gameState
            transition =
               game.transition
         in
         { game | gameState =
                     if gameState == NoCheck  
                        then 
                           Check
                        else 
                           NoCheck
                , transition =
                  { transition | animationOn = if game.choiceState == NoChoice
                                                   then
                                                      transition.animationOn
                                                   else
                                                      not transition.animationOn
                               , graph =  if game.choiceState == NoChoice
                                             then
                                                transition.graph
                                             else
                                                Graph.changeGlowVertex True 1 transition.graph
                  }

                , graphB =    if game.choiceState == NoChoice
                              then
                                 game.graphB
                              else
                              Graph.changeGlowVertex True 1 game.graphB

                , graphC =    if game.choiceState == NoChoice
                              then
                                 game.graphC
                              else
                              Graph.changeGlowVertex True 1 game.graphC
         }

      IsoReset ->
         isomorphicGame
      _ ->
         let
            transition =
               animateIsomorphicGameTrans msg game.transition
         in
         { game | transition = transition }

-- changeGlowVertex : Bool -> Int -> Graph -> Graph

animateIsomorphicGameTrans : Msg -> InPlaceTransition -> InPlaceTransition
animateIsomorphicGameTrans msg inPlaceTran =
   case msg of
       TimeDelta delta ->
           case inPlaceTran.animationOn of
               True ->
                   Graph.executeInPlaceShapeTransition delta inPlaceTran
               False ->
                   inPlaceTran


       AnimationStartOver ->
           { inPlaceTran
               | graph = inPlaceTran.backupGraph
               , time = 0.0
           }

       _ ->
           inPlaceTran
      

animateIsomorphicTransition : Msg -> ShapeTransition -> ShapeTransition
animateIsomorphicTransition msg shapeTransition =
   case msg of
       TimeDelta delta ->
           case shapeTransition.animationOn of
               True ->
                   Graph.executeShapeTransition delta shapeTransition
               False ->
                   shapeTransition
   
       HoverOver name ->
           { shapeTransition
               | graphA =
                     shapeTransition.graphA
                     |> Graph.makeUnglowAllVertices
                     |> Graph.changeGlowVertex True name
               , graphB = 
                     shapeTransition.graphB
                     |> Graph.makeUnglowAllVertices
                     |> Graph.changeGlowVertex True name
           }
   
       MouseOut name ->
           { shapeTransition
             | graphA = 
                  shapeTransition.graphA
                  |> Graph.changeGlowVertex False name
             , graphB = 
                  shapeTransition.graphB
                  |> Graph.changeGlowVertex False name
           }
   
       ToggleVertexStatus name ->
           { shapeTransition
               | graphA = 
                     shapeTransition.graphA
                     |> Graph.makeUnglowAllVerticesBut name
                     |> Graph.toggleGlowVertex name
               , graphB = 
                     shapeTransition.graphB
                     |> Graph.makeUnglowAllVerticesBut name
                     |> Graph.toggleGlowVertex name
           }
   
       AnimationToggle ->
           { shapeTransition
               | animationOn = not shapeTransition.animationOn
           }
   
       AnimationStartOver ->
           { shapeTransition
               | graphB = shapeTransition.graphA
               , time = 0.0
           }

       Other ->
           shapeTransition

       NextTopic ->
           shapeTransition
       _ ->
           shapeTransition


drawGraph g =
    let
        ( specialEdges, normalEdges ) =
            Graph.seperateEdges g

        haloVertices = Graph.getHaloVertices g specialEdges

        selectedVertices =
            List.filter (\ver -> ver.glow) g.vertices
         
    in
    List.map Graph.drawEdge normalEdges
        ++ List.map Graph.drawSpecialEdge specialEdges
        ++ List.map Graph.drawGoldenCircle haloVertices
        ++ List.map Graph.drawVertex g.vertices
        ++ List.map Graph.drawSelectedVertex selectedVertices
        ++ List.map Graph.writeVertexName g.vertices

paneOne graphA graphB =
    Graph.displaySvg ((drawGraph graphA) ++ (drawGraph graphB))

-- isomorphicDisplay : IsomorphicTopic -> 
isomorphicDisplay topic =
   case topic.topicState of
      Transition ->
         Graph.displaySvg ( (drawGraph topic.shapeTransition.graphA) 
                            ++ (drawGraph topic.shapeTransition.graphB) )
      Game ->
         let
            game =
               topic.isomorphicGame

            choice = 
               game.choiceState

            gameState =
               game.gameState
         in
         Graph.displaySvg ( (drawGraph topic.isomorphicGame.transition.graph) 
                            --++ (Graph.drawCheckBoxes)
                            ++ (drawSquares choice gameState)
                            ++ (drawGraph topic.isomorphicGame.graphB) 
                            ++ (drawGraph topic.isomorphicGame.graphC) 
                          )



linearGridLeft =
    linearGrid 4 (vec3 150 250 0) (vec3 0 120 0)


linearGridRight =
    linearGrid 4 (vec3 250 250 0) (vec3 0 120 0)

linearGridLeftInPlace =
    linearGrid 4 (vec3 50 50 0) (vec3 0 120 0)


linearGridRightInPlace =
    linearGrid 4 (vec3 150 50 0) (vec3 0 120 0)
--linearGridLeftInPlace =
--    linearGrid 4 (vec3 50 150 0) (vec3 0 120 0)
--
--
--linearGridRightInPlace =
--    linearGrid 4 (vec3 150 150 0) (vec3 0 120 0)
------------------------------------------

linearGridLeftInPlaceSecond =
    linearGrid 4 (vec3 250 50 0) (vec3 0 120 0)


linearGridRightInPlaceSecond =
    linearGrid 4 (vec3 350 50 0) (vec3 0 120 0)

linearGridLeftInPlaceThird =
    linearGrid 4 (vec3 250 250 0) (vec3 0 120 0)


linearGridRightInPlaceThird =
    linearGrid 4 (vec3 350 250 0) (vec3 0 120 0)




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

bipartiteGridInPlace =
    let
        leftTupled =
            List.map2 (\x y -> ( x, y )) setLeft linearGridLeftInPlace

        rightTupled =
            List.map2 (\x y -> ( x, y )) setRight linearGridRightInPlace

        totalGrid =
            leftTupled ++ rightTupled
    in
    List.map (\( x, y ) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) totalGrid)

bipartiteGridInPlaceSecond =
    let
        leftTupled =
            List.map2 (\x y -> ( x, y )) setLeft linearGridLeftInPlaceSecond

        rightTupled =
            List.map2 (\x y -> ( x, y )) setRight linearGridRightInPlaceSecond

        totalGrid =
            leftTupled ++ rightTupled
    in
    List.map (\( x, y ) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) totalGrid)


bipartiteGridInPlaceThird =
    let
        leftTupled =
            List.map2 (\x y -> ( x, y )) setLeft linearGridLeftInPlaceThird

        rightTupled =
            List.map2 (\x y -> ( x, y )) setRight linearGridRightInPlaceThird

        totalGrid =
            leftTupled ++ rightTupled
    in
    List.map (\( x, y ) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) totalGrid)



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


type ScreenSize
   = Big
   | Standard
   | Small
   | Smaller


explanationOne : IsomorphicTopic -> Bool -> DisplaySize -> ELE.Element Msg
explanationOne isoTopic helpStatus displaySize =
   case isoTopic.topicState of
      Transition ->
         explanationTransition isoTopic.shapeTransition helpStatus displaySize
      Game ->
         explanationGame isoTopic.isomorphicGame helpStatus displaySize

explanationGame :  IsomorphicGame -> Bool -> DisplaySize -> ELE.Element Msg
explanationGame game helpStatus displaySize =
      let
         emph =
            emphForScreen displaySize.deviceType
      in
      ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.height ELE.fill
         , ELE.spacing 20
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.width (ELE.fill |> ELE.maximum (displaySize.width))
         , ELE.scrollbarY
         ]
         <|
         [  ELE.el
               [Font.size (getFontSize Head displaySize.deviceType), Font.heavy]
               (ELE.text "Graph Isomorphism")
         ,  ELE.paragraph
               [ELE.spacing 8] 
               [ELE.text isomorphismExplanation]
         , ELE.paragraph
               []
               [ emph CuteBlue
                     """
                     Choose 
                     """
               , ELE.text
                     """
                     which graph is isomorphic to the first one
                     by 
                     """
               , emph CuteBlue
                     """
                     clicking 
                     """
               , ELE.text
                     """
                     on them.
                     """
               ]

         , gameButtons displaySize

         ]

         ++ (List.map (ELE.paragraph [])
               (gameStatusExplanation game displaySize))


         ++ [ lowerNavigation "Tree Width" "Max Cut"
            ]



gameStatusExplanation : IsomorphicGame -> DisplaySize -> List (List (ELE.Element Msg))
gameStatusExplanation game displaySize =
   let
      emph =
         emphForScreen displaySize.deviceType
      makeAChoice =
         [ ELE.text  
            """
            Make a choice by 
            """
         , emph CuteBlue
            """
            clicking 
            """
         , ELE.text
            """
            on one of the boxed graphs and then
            """
         , emph CuteBlue
            """
            press 
            """
         , ELE.text
            """
            the check button.
            """
         ]
      choiceMadeFirst =
         [ ELE.text
               """
               You have 
               """
         , emph CuteBlue
               """
               chosen 
               """
         , ELE.text
               """
               the 
               """
         , emph CuteGreen
               """
               first 
               """
         , ELE.text
               """
               graph.
               """
         ]
      choiceMadeSecond =
         [ ELE.text
            """
            You have 
            """
         , emph CuteBlue
            """
            chosen 
            """
         , ELE.text
            """
            the 
            """
         , emph CuteGreen
            """
            second 
            """
         , ELE.text
            """
            graph.
            """
        ]
      youAreWrong =
         [ ELE.text
            """
            It's 
            """
         , emph Pink
            """
            incorrect! 
            """
         , ELE.text
            """
            Maybe, go back to the explanation.
            """
         ]

      youAreRight =
         [ ELE.text
               """
               Yes it's 
               """
         , emph Pink
               """
               correct! 
               """
         , ELE.text
               """
               Well done.
               """
         ]

      choiceText =
         case (game.choiceState, game.gameState) of
            (NoChoice, _) ->
               makeAChoice
            (FirstGraph, _) ->
               choiceMadeFirst 
            (SecondGraph, _) ->
               choiceMadeSecond 

      checkText =
         case (game.choiceState, game.gameState) of
            (FirstGraph, Check) ->
               youAreRight
            (SecondGraph, Check) ->
               youAreWrong
            (_, _) ->
               [ ELE.none ]
      in
      [ choiceText
      , checkText
      ]

gameButtons displaySize =
   ELE.row
      [ ELE.centerX
      , ELE.spacing (displaySize.width//10)
      ]
      [ isoCheckButton
      , isoResetButton
      , taskButton True
      ]
explanationTransition : ShapeTransition -> Bool -> DisplaySize -> ELE.Element Msg
explanationTransition shapeTransition helpStatus displaySize =
      let
         emph =
            emphForScreen displaySize.deviceType
      in
      ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         , ELE.height ELE.fill
         , ELE.spacing 20
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         , ELE.width (ELE.fill |> ELE.maximum (displaySize.width))
         , ELE.scrollbarY
         ]
         <|
         [  ELE.el
               [Font.size (getFontSize Head displaySize.deviceType), Font.heavy]
               (ELE.text "Graph Isomorphism")
         ,  ELE.paragraph
               [ELE.spacing 8] 
               [ELE.text isomorphismExplanation]

         , ELE.paragraph
               []
               [ ELE.text "You should now press the "
               , emph CuteBlue "Play"
               , ELE.text 
                     """
                      button, to set the animation rolling.
                     Press the 
                     """
               , emph CuteBlue "Restart "
               , ELE.text
                     """
                     button to see it all over again.
                     """
               ]

         , mediaButtons shapeTransition displaySize


         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     Go ahead and put your mouse over a 
                     """
               , emph CuteGreen "vertex"


               , ELE.text

                     """
                     of the graph. 
                     Or 
                     """
               , emph CuteBlue
                     "press"
               , ELE.text
                     """
                      a number on the keyboard corresponding to a Vertex number.
                     """
               ]

         ]

         ++  
            (makeStory displaySize.deviceType shapeTransition helpStatus)
         ++


         [ lowerNavigation "Tree Width" "Max Cut" ]

makeStory : DeviceType -> ShapeTransition -> Bool -> List (ELE.Element Msg)
makeStory deviceType shapeTransition helpStatus =
    let
        glowing_vertices =
            List.filter (\ver -> ver.glow) shapeTransition.graphB.vertices
        
        emph =
            emphForScreen deviceType

        putyourmouse =
            """
            Go ahead and put your mouse over a vertex of the graph.
            Or press a number on the keyboard corresponding to a Vertex number.
            """

        ( specialEdges, _ ) =
            Graph.seperateEdges shapeTransition.graphB

        relatedVertices =
            Graph.getHaloVertices shapeTransition.graphB specialEdges

        connectedToThis v =
            "And connected to vertex {{ }} are the vertices " |> String.Format.value (String.fromInt <| v.name)

        whichYouCanSee =
            " Which you can see is true for both graphs."

        listOfStories =
            case glowing_vertices of
                [] ->
                    [ ELE.none ]

                x :: xs ->
                    [ ELE.text "You have selected "
                    , emph CuteGreen "Vertex "
                    , emph Pink (String.fromInt x.name)
                    , ELE.text ". Connected to this vertex are" 
                    , emph CuteGreen " Vertices "
                    , emph Pink <| Graph.getStringFromVertices relatedVertices
                    , ELE.text "."
                    , ELE.text whichYouCanSee
                    ]


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
         [ ELE.paragraph [] listOfStories
         , ELE.paragraph [] [ELE.text footer]
         ]
      else
         [ helpParagraph IsomorphismHelp]
mediaButtons : ShapeTransition -> DisplaySize -> ELE.Element Msg
mediaButtons shapeTransition displaySize =
   ELE.row
      [ ELE.centerX
      , ELE.spacing (displaySize.width//10)
      ]
      [  playButton shapeTransition.animationOn
      ,  resetButton
      ,  taskButton False
      ]

drawSquares : ChoiceState -> GameState -> List (S.Svg Msg)
drawSquares choice gameState =
   let
      circleOneColor =
         case choice of
            FirstGraph ->
               "blue"
            _ ->
               "gray"

      circleTwoColor =
         case choice of
            SecondGraph ->
               "blue"
            _ ->
               "gray"


      squareOneColor =
          case (choice, gameState) of
               (FirstGraph, Check) ->
                  "green"
               (SecondGraph, Check) ->
                  "green"
               (_, _) ->
                  "gray"

      squareTwoColor =
          case (choice, gameState) of
               (FirstGraph, Check) ->
                  "red"
               (SecondGraph, Check) ->
                  "red"
               (_, _) ->
                  "gray"

   in
   [
      S.rect
         [ SA.x "220"
         , SA.y "30"
         , SA.width "155"
         , SA.height "160"
         , SA.opacity "0.2"
         , SA.stroke "white"
         , SE.onClick IsoChoiceOne
         , SA.fill squareOneColor
         ]
         []
   , 
      S.rect
         [ SA.x "220"
         , SA.y "230"
         , SA.width "155"
         , SA.height "160"
         , SA.opacity "0.2"
         , SA.stroke "white"
         , SE.onClick IsoChoiceTwo
         , SA.fill squareTwoColor
         ]
         []

    , S.circle
         [ SA.cx "229"
         , SA.cy "180"
         , SA.r "5"
         , SA.stroke "white"
         , SA.opacity "0.2"
         , SA.fill circleOneColor
         , SE.onClick IsoChoiceOne
         ]
         []

    , S.circle
         [ SA.cx "229"
         , SA.cy "380"
         , SA.r "5"
         , SA.stroke "white"
         , SA.opacity "0.2"
         , SA.fill circleTwoColor
         , SE.onClick IsoChoiceTwo
         ]
         []
    ]
