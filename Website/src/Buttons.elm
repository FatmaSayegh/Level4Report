module Buttons exposing (..)

import Element as ELE
import Element.Input as Input
import Element.Border as Border
import Ant.Icon as Ant
import Ant.Icons as Icons
import Messages exposing (Msg(..))
import Element.Font as Font
import Element.Background as Background
import Element.Events as Events
import Element.Cursor as Cursor
import FontSize exposing
               ( getFontSize
               , FontSize(..)
               , FontColor(..)
               , giveFontColor
               , emphForScreen
               , DisplaySize
               , DeviceType(..)
               )

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

linkedInFatmaButton =
   let
      linkedIn =
         ELE.newTabLink
            []
            { url = "https://www.linkedin.com/in/fatma-alsayegh-1a060220a/"
            --, label = Icons.linkedinOutlined [ Ant.width 40, Ant.height 40 ]
            , label = Icons.linkedinFilled [ Ant.width 40, Ant.height 40 ]
            }
   in
   buttonWrap "LinkedIn" linkedIn

websiteSofiat =
   let
      website =
         ELE.newTabLink
            []
            { url = "https://www.dcs.gla.ac.uk/~sofiat/"
            ,  label =  
                       ELE.el [Font.size 40] (ELE.text "W")
            }
   in
   buttonWrap "Website" website

linkedInSofiat =
   let
      linkedIn =
         ELE.newTabLink
            []
            { url = "https://www.linkedin.com/in/sofiatolaosebikan/"
            --, label = Icons.linkedinOutlined [ Ant.width 40, Ant.height 40 ]
            , label = Icons.linkedinFilled [ Ant.width 40, Ant.height 40 ]
            }
   in
   buttonWrap "LinkedIn" linkedIn
gitHubButton =
   let
      github =
         ELE.newTabLink
            []
            { url = "https://github.com/FatmaSayegh/"
            , label = Icons.githubFilled [ Ant.width 40, Ant.height 40 ]
            }
   in
   buttonWrap "GitHub" github

projectGitHub =
   let
      github =
         ELE.newTabLink
            []
            { url = "https://github.com/FatmaSayegh/Level4Report"
            , label = Icons.githubFilled [ Ant.width 40, Ant.height 40 ]
            }
   in
   buttonWrap "GitHub" github

fatmaButtons =
   ELE.row [ ELE.centerX, ELE.spacing 20]
      [ linkedInFatmaButton
      , gitHubButton
      ]

sofiatButtons =
   ELE.row [ ELE.centerX, ELE.spacing 20]
      [ linkedInSofiat
      , websiteSofiat
      ]


projectLinks =
   ELE.row [ ELE.centerX, ELE.spacing 20]
      [ projectGitHub
      ]

treeWidthButtonRow : Bool -> DisplaySize -> ELE.Element Msg
treeWidthButtonRow  isPreviousActive displaySize =
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
      [ ELE.centerX
      , ELE.spacing (displaySize.width//10)
      ]
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

isoCheckButton : ELE.Element Msg
isoCheckButton =
         buttonWrap "Check your Choice" 
         <| Input.button
              [
                ELE.centerX
              ] 
              { onPress = Just IsoCheck
              , label = Icons.checkOutlined [ Ant.width 70, Ant.height 50 ]
              }

isoResetButton : ELE.Element Msg
isoResetButton =
         buttonWrap "Start Over" 
         <| Input.button
              [
                ELE.centerX
              ] 
              { onPress = Just IsoReset
              , label = Icons.rollbackOutlined [ Ant.width 70, Ant.height 50 ]
              --, label = Icons.bulbOutlined [ Ant.width 70, Ant.height 50 ]
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

homeButton : Int -> ELE.Element Msg
homeButton size =
   let
      theButton =
         Input.button
            [ELE.centerX, Font.color (ELE.rgb 0.8 0.8 0.8)]
            {  onPress = Just GotoHome  
            ,  label =  
                       Icons.homeOutlined [ Ant.width size, Ant.height size ]
            }

    in
    buttonWrap "Home/Contents" theButton

aboutButton : Int -> ELE.Element Msg
aboutButton size =
   let
      theButton =
         Input.button
            [ELE.centerX, Font.color (ELE.rgb 0.8 0.8 0.8)]
            {  onPress = Just GotoAbout  
            ,  label =  
                       ELE.el [Font.size size] (ELE.text "A")
            }

    in
    buttonWrap "About" theButton

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


taskButton : Bool -> ELE.Element Msg
taskButton status =
   let
      text =
         if status
            then
               "Previous Animation"
            else
               "Next Task"
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
    buttonWrap text theButton


tryDifferent : ELE.Element Msg
tryDifferent =
    let 
      theButton =
         Input.button
            [
               Border.rounded 100
            ,  ELE.centerX
            ] 
            { onPress = Just NextAnimation
            , label = Icons.forwardOutlined [ Ant.width 50, Ant.height 50 ]
            }
    in
    buttonWrap "Try a different task" theButton

nextTask : ELE.Element Msg
nextTask =
    let 
      theButton =
         Input.button
            [
               Border.rounded 100
            ,  ELE.centerX
            ] 
            { onPress = Just NextAnimation
            , label = Icons.forwardOutlined [ Ant.width 50, Ant.height 50 ]
            }
    in
    buttonWrap "Next Task" theButton

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
         
     ,  ELE.el 
            [ ELE.alignLeft
            , Events.onClick PreviousTopic 
            , Cursor.pointer
            ] 
            <| ELE.text leftTitle    

     ,   Input.button
         [
            Border.rounded 100
         ,  ELE.centerX
         ] 
         { onPress = Just ToggleHelpStatus
         , label = Icons.infoCircleOutlined [ Ant.width 40, Ant.height 40 ]
         }

     ,  ELE.el 
            [ ELE.alignRight
            , Events.onClick NextTopic 
            , Cursor.pointer
            ] 
            <| ELE.text rightTitle    
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

