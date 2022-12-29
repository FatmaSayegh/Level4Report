module Main exposing (..)

import Browser
import Browser.Events as E
import Color exposing (Color)
import Explanation exposing (..)
import Html as H exposing (div, h1, p, text)
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (..)
import Json.Decode as Decode
import Math.Matrix4 as M4 exposing (..)
import Math.Vector3 exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)
import String.Format
import Point2d as Pt
import LineSegment2d as Ln
import Length as Len
import Element as ELE
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Element.Input as Input
import Ant.Icon as Ant
import Ant.Icons as Icons
import Messages exposing (Msg(..))
import Graph as GR





-- Main Program
-- main is the main program
-- init will initialize the model
-- view uses the model to populate the app
-- update updates the model
-- subscription subscribes to the clock


main : Program () SuperModel Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscription
        }



-- Model of the app
-- Model contains the data model of the app.
-- Model has currently has 2 graphs
-- and a grid, graphB will transform into slowly.

type alias SuperModel =
   { helpStatus : Bool
   , model : Model
   }

type Model =
   Isomorphic ShapeTransition
   | MaxCut MaxCutTransition
   | GraphColoring ColorDisplay
   | VertexCover VertexCoverDisplay
   | TreeWidth TreeWidthDisplay





         



init : () -> ( SuperModel, Cmd Msg )
init _ =
    let
        initialGraph =
            makeGraph (PolygonCycleDoll 4) (vec3 200 100 0) (vec3 80 80 0) (pi / 4)

        shapeTransition =
            { graphA = initialGraph
            , graphB = initialGraph
            , finalGrid = bipartiteGrid
            , animationOn = False
            , specialToken = NoToken
            }
        model = ( Isomorphic shapeTransition)
    in
    ({ helpStatus = False
    , model = model
    }, Cmd.none)





-- Subscribing to Animation frame clock.
-- Generates a Msg which can be used by update function


subscription : SuperModel -> Sub Msg
subscription _ =
    Sub.batch
        [ E.onAnimationFrameDelta TimeDelta
        , E.onKeyPress keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map keyToMsg (Decode.field "key" Decode.string)


keyToMsg : String -> Msg
keyToMsg value =
    case String.uncons value of
        Just ( char, "" ) ->
            case (Char.isDigit char) of
               True ->
                  String.cons char "" |> String.toInt |> chooseVertexFromInt
               False ->
                  case char of
                     'r' ->
                         AnimationStartOver

                     'p' ->
                         AnimationToggle

                     'n' ->
                         NextTopic

                     'N' ->
                         PreviousTopic

                     'l' ->
                         MaxCutLine 
                     'w' ->
                         VertexNonColor
                     't' ->
                         NextAnimation
                     'T' ->
                         PreviousTreeWidthAnimation
                     'h' ->
                         ToggleHelpStatus
                     _ ->
                         Other
        _ ->
            Other


chooseVertexFromInt : Maybe Int -> Msg
chooseVertexFromInt x =
   case x of
      Nothing
         -> Other
      Just name
         -> ToggleVertexStatus name



-- Update the model
-- Update function takes in messages from the webpage or the subscriber
-- and uses them to modify the model
-- 1. With every animation clock tick, it changes the graph to move towards
--    a specified grid.
-- 2. With MouseOver a vertex it makes that vertices' incident edges glow.
-- 3. With MouseOut from a vertex it makes that vertices' incident edges not glow.


update : Msg -> SuperModel -> ( SuperModel, Cmd Msg )
update msg superModel =
    let
      model = superModel.model
      newModel =
         case msg of
           NextTopic ->
              case model of
                 Isomorphic x ->
                    ( MaxCut maxCutTransition)
                 MaxCut x ->
                    (GraphColoring colorDisplay)
                 GraphColoring x ->
                    ( VertexCover vertexCoverDisplay)
                 VertexCover x ->
                    ( TreeWidth treeWidthDisplay)
                 TreeWidth x ->
                    ( Isomorphic isomorphicTransition)

           PreviousTopic ->
              case model of
                 Isomorphic x ->
                    ( TreeWidth treeWidthDisplay)
                 TreeWidth x ->
                    ( VertexCover vertexCoverDisplay)
                 MaxCut x ->
                    ( Isomorphic isomorphicTransition)
                 GraphColoring x ->
                    ( MaxCut maxCutTransition)
                 VertexCover x ->
                    (GraphColoring colorDisplay)
           _ ->
              case model of
                Isomorphic shapeTransition ->
                   ( Isomorphic (animateIsomorphicTransition msg shapeTransition))
                MaxCut maxcutTrans ->
                   ( MaxCut (animateMaxCutCompound msg maxcutTrans))
                GraphColoring display ->
                   ( goColor display msg)
                VertexCover display ->
                   ( goCover display msg)
                TreeWidth display ->
                   ( goTree display msg)
      helpStatus =
         case msg of
            ToggleHelpStatus ->
               not superModel.helpStatus
            TimeDelta _ ->
               superModel.helpStatus
            _ ->
               False
            
   in
   ({ superModel | model = newModel, helpStatus = helpStatus }, Cmd.none)



toggleToken : Token -> Token
toggleToken token =
   case token of
      MakeKCut ->
         NoToken
      NoToken ->
         MakeKCut




-- View the Model
-- This function is responsible for the actual rendering
-- of the webpage. Any change in the model by update function is reflected in the
-- webpage as view works with the latest model.

layOutOptions =
   { options =
      [ ELE.focusStyle
         { borderColor = Nothing
         , backgroundColor = Nothing
         , shadow = Nothing
         }
       ]
   } 

--layOutAttributes = [ELE.width ELE.fill, ELE.height ELE.fill]
layOutAttributes = 
                  -- [ ELE.width ELE.fill
                   [ ELE.height ELE.fill
                   , Background.color <| ELE.rgb 0.2 0.2 0.2
                   , ELE.padding 30
                   ]

displayColumn svgHtml =
   ELE.column
      [ Font.color (ELE.rgb 1 1 1)
      , ELE.height ELE.fill
      , ELE.width ELE.fill
      , Background.color <| ELE.rgb 0.2 0.2 0.2
      ] [ELE.html svgHtml]


view superModel =
   case superModel.model of
      Isomorphic shapeTransition ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ELE.width ELE.fill
                  --, Background.color <| ELE.rgb 44 44 44
                  ]

                  [ displayColumn (paneOne shapeTransition.graphA shapeTransition.graphB)
                  , explanationOne shapeTransition superModel.helpStatus
                  ]
            )


      MaxCut maxCutTrans ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneTwo maxCutTrans) 
                  , explanationTwo maxCutTrans superModel.helpStatus
                  ]
            )

      GraphColoring display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneThree display) 
                  , explanationColoring display superModel.helpStatus
                  ]
            )

      VertexCover display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneFour display) 
                  , explanationCover display superModel.helpStatus
                  ]
            )

      TreeWidth display ->
         ELE.layoutWith 
            layOutOptions
            layOutAttributes
            ( ELE.row
                  [ ELE.width ELE.fill]

                  [ displayColumn (paneTree display) 
                  , explanationWidth display superModel.helpStatus
                  ]
            )







-- Move a graph towards grid points.
-- This function takes as inputs a graph, and a target grid
-- First an intermediate grid is created near the input graph, which has the placeholder for
-- the vertices a little bit towards the final destination.
-- then the original graph is morphed into the intermediate grid.


type alias Size =
    Int

-- put edges first and then vertices
-- and produces a single list


findTwoPositions : List Vertex -> ( (Int, Int, Int), (Int, Int, Int) ) -> Maybe (Vec3, Vec3)
findTwoPositions vs x =
   case x of
      ((a1, b1, c1), (a2, b2, c2)) ->
         let 
            pos1 = findCenterOfTriple a1 b1 c1 vs
            pos2 = findCenterOfTriple a2 b2 c2 vs
         in
         case (pos1, pos2) of
            (Nothing, _) ->
               Nothing
            (_, Nothing) ->
               Nothing
            (Just p1, Just p2) ->
               Just (p1, p2)
            
      

findCenterOfTriple : Int -> Int -> Int -> List Vertex -> Maybe Vec3
findCenterOfTriple a b c vs =
   case (lookUpVertex a vs, lookUpVertex b vs, lookUpVertex c vs) of
      (Nothing, _, _) ->
         Nothing
      (_, Nothing, _) ->
         Nothing
      (_, _, Nothing) ->
         Nothing
      (Just v1 , Just v2, Just v3) ->
            Just <| 
            ( Math.Vector3.add v1.pos v2.pos
            |> Math.Vector3.add v3.pos
            |> Math.Vector3.scale 0.333
            )

      
            
type HelpKind =            
   IsomorphismHelp
   | MaxCutHelp
   | GraphColoringHelp
   | VertexCoverHelp
   | TreeWidthHelp

helpParagraph helpkind =
   let
      helpdetails =
         case helpkind of
            IsomorphismHelp ->
               helpIsomorphic
            MaxCutHelp ->
               helpMaxCut
            GraphColoringHelp ->
               helpGraphColor
            VertexCoverHelp ->
               helpVertexCover
            TreeWidthHelp ->
               helpTreeWidth

    in        
    ELE.el [] ( 
                ELE.column 
                   [ Font.color (ELE.rgb 0.8 1 0.8)
                   --, ELE.height ELE.fill
                   --, ELE.width ELE.fill
                   , Font.size 18
                   , Background.color <| ELE.rgb 0.2 0.2 0.2
                   ] 
                   helpdetails
              )

helpDetails =
   [ ELE.el [ELE.paddingXY 5 10, Font.size 20] (ELE.text "Keyboard Shorcuts:")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "h: Toggle Help")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "p: Toggle between pause and play animation")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "r: Restart animation")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "n: Next topic")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "N: Previous topic")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "t: For Max Cut: Next animation")
   ]

helpGraphColor =
   [ ELE.el [ELE.paddingXY 5 10, Font.bold] (ELE.text "Mouse Funcionality:")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "Click color on the palette: Select Color")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "Click Vertex: Color the vertex with the selected color")
   ]
helpVertexCover =
   [ ELE.el [ELE.paddingXY 5 10, Font.size 20] (ELE.text "Keyboard Shorcuts:")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "h: Toggle Help")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "1,2,3,4 ..... 8: Select/Unselect Vertex")
   ]
helpTreeWidth =
   [ ELE.el [ELE.paddingXY 5 10] (ELE.text "Keyboard Shorcuts:")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "h: Toggle Help")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "t: Next animation in tree width")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "T: Previous animation in tree width")
   ]
helpMaxCut =
   [ ELE.el [ELE.paddingXY 5 10, Font.bold] (ELE.text "Keyboard Shorcuts:")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "h: Toggle Help")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "p: Toggle between pause and play animation")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "r: Restart animation")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "n: Next topic")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "N: Previous topic")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "t: Next animation")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "l: Draw Max cut line(s)")
   ]

helpIsomorphic =
   [ ELE.el [ELE.paddingXY 5 10, Font.bold] (ELE.text "Keyboard Shorcuts:")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "h: Toggle Help")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "p: Toggle between pause and play animation")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "r: Restart animation")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "1,2,3 ..... 8: Select/Unselect a vertex")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "n: Next topic")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "N: Previous topic")
   , ELE.el [ELE.paddingXY 5 10, Font.bold] (ELE.text "Mouse Funcionality:")
   , ELE.el [ELE.paddingXY 5 2] (ELE.text "Hover Over Vertex: Select Vertex ")
   ]




mediaButtons : ShapeTransition -> ELE.Element Msg
mediaButtons shapeTransition =
   ELE.row
      [ELE.spacing 90, ELE.paddingXY 300 40]
      [  playButton shapeTransition.animationOn
      ,  resetButton
      ]

mediaButtonsForMaxCut : ShapeTransition -> ELE.Element Msg
mediaButtonsForMaxCut shapeTransition =
   let
      forward =
         Input.button
            [
               Border.rounded 100
            ,  ELE.centerX
            ] 
            { onPress = Just NextAnimation
            , label = Icons.forwardOutlined [ Ant.width 40, Ant.height 40 ]
            }
   in
   ELE.row
      [ELE.spacing 90, ELE.paddingXY 300 40]
      [  playButton shapeTransition.animationOn
      ,  resetButton
      ,  forward  
      ]


playButton : Bool -> ELE.Element Msg
playButton animationOn =
   Input.button
      []
      {  onPress = Just AnimationToggle  
      ,  label = if animationOn  
                 then   Icons.pauseOutlined [ Ant.width 50, Ant.height 50 ]
                 else   Icons.caretRightOutlined [ Ant.width 50, Ant.height 50 ]
      }

resetButton : ELE.Element Msg
resetButton =
   Input.button
         []
         {  onPress = Just AnimationStartOver
         ,  label = Icons.rollbackOutlined [ Ant.width 50, Ant.height 50 ]
 
         }
                        
               

lowerNavigation : String -> String -> ELE.Element Msg
lowerNavigation leftTitle rightTitle =
   ELE.row
      [ ELE.alignBottom 
      , ELE.width ELE.fill
      , ELE.padding 20
      , ELE.spacing 20
      ]
      [
         Input.button
         [
            Border.rounded 100
         ,  ELE.alignLeft
         ] 
         { onPress = Just PreviousTopic
         , label = Icons.verticalRightOutlined [ Ant.width 40, Ant.height 40 ]
         }
         
     ,  ELE.el [ ELE.alignLeft ] <| ELE.text leftTitle    

     ,   Input.button
         [
            Border.rounded 100
         ,  ELE.centerX
         ] 
         { onPress = Just ToggleHelpStatus
         , label = Icons.infoCircleOutlined [ Ant.width 40, Ant.height 40 ]
         }

     ,  ELE.el [ ELE.alignRight ] <| ELE.text rightTitle    
     ,
         Input.button
         [
            Border.rounded 100
         ,  ELE.alignRight
         ] 
         { onPress = Just NextTopic
         , label = Icons.verticalLeftOutlined [ Ant.width 40, Ant.height 40 ]
         }
     ]
