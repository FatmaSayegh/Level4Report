module Buttons exposing (..)

import Element as ELE
import Element.Input as Input
import Element.Border as Border
import Ant.Icon as Ant
import Ant.Icons as Icons
import Messages exposing (Msg(..))
import Element.Font as Font
import Element.Background as Background

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


treeWidthButtonRow : Bool -> ELE.Element Msg
treeWidthButtonRow  isPreviousActive =
   let
      forward =
         buttonWrap "Next animation"
         <| Input.button
               [
                  Border.rounded 100
               ,  ELE.centerX
               ] 
               --{ onPress = Just NextTreeWidthAnimation
               { onPress = Just NextAnimation
               , label = Icons.forwardOutlined [ Ant.width 40, Ant.height 40 ]
               }

      backward =
         buttonWrap "Previous animation"
         <| Input.button
               [
                  Border.rounded 100
               ,  ELE.centerX
               ] 
               { onPress = Just PreviousTreeWidthAnimation
               , label = Icons.backwardOutlined [ Ant.width 40, Ant.height 40 ]
               }
      backwardDead =
         buttonWrap "Previous animation"
         <| Input.button
               [
                  Border.rounded 100
               ,  ELE.centerX
               ,  Font.color (ELE.rgb 0.4 0.4 0.4)
               ] 
               { onPress = Nothing
               , label = Icons.backwardOutlined [ Ant.width 40, Ant.height 40 ]
               }
   in
   ELE.row
      [ELE.spacing 90, ELE.paddingXY 300 40]
      [  if isPreviousActive then backward else backwardDead
      ,  forward
      ]

unColorButton : ELE.Element Msg
unColorButton =
         buttonWrap "Uncolor all Vertices" 
         <| Input.button
              [
                ELE.centerX
              ] 
              { onPress = Just VertexNonColor
              , label = Icons.rollbackOutlined [ Ant.width 70, Ant.height 50 ]
              }

playButton : Bool -> ELE.Element Msg
playButton animationOn =
   let
      theButton =
         Input.button
            []
            {  onPress = Just AnimationToggle  
            ,  label = if animationOn  
                       then   Icons.pauseOutlined [ Ant.width 50, Ant.height 50 ]
                       else   Icons.caretRightOutlined [ Ant.width 50, Ant.height 50 ]
            }

    in
    buttonWrap "Play/Pause" theButton


resetButton : ELE.Element Msg
resetButton =
   let
      theButton =
         Input.button
               []
               {  onPress = Just AnimationStartOver
               ,  label = Icons.rollbackOutlined [ Ant.width 50, Ant.height 50 ]
 
               }
    in
    buttonWrap "Restart" theButton
                        
               
forwardButton : ELE.Element Msg
forwardButton =
   let
      theButton =
         Input.button
            [
               Border.rounded 100
            ,  ELE.centerX
            ] 
            { onPress = Just NextAnimation
            , label = Icons.forwardOutlined [ Ant.width 40, Ant.height 40 ]
            }
    in
    buttonWrap "Next Animation" theButton

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

