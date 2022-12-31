module Messages exposing (Msg(..))

import Color exposing (Color)
-- Msg
-- This data type contains the kinds of messages
-- the html page or the or a subscriber (animation clock in this app) may give to the elm-runtime.


type Msg
    = TimeDelta Float
    | HoverOver Int
    | MouseOut Int
    | VertexClicked Int
    | AnimationToggle
    | AnimationStartOver
    | ToggleVertexStatus Int
    | NextTopic
    | PreviousTopic
    | MaxCutLine
    | ColoringSelectColor Color
    | VertexNonColor
    | NextAnimation 
    | PreviousTreeWidthAnimation
    | ToggleHelpStatus
    | GotoHome
    | GotoIsomorphism
    | GotoMaxkCut
    | GotoColoring
    | GotoCover
    | GotoTreeWidth
    | Other
