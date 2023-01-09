module FontSize exposing 
   ( getFontSize
   , FontSize(..)
   , FontColor(..)
   , giveFontColor
   , emph
   )

import Element as ELE
import Element.Font as Font

type FontSize
   = Head
   | Normal

getFontSize : FontSize -> Int -> Int
getFontSize f width =
   case f of
      Head ->
         if width > 1800 then
            30
         else if width > 1500 then
            25
         else if width > 1100 then
            20
         else if width > 800 then
            18
         else
            15
      Normal ->
         if width > 1800 then
            18
         else if width > 1500 then
            17
         else if width > 1100 then
            16
         else if width > 800 then
            15
         else
            14

type FontColor
   = Pink
   | CuteGreen
   | CuteBlue


giveFontColor : FontColor -> ELE.Color
giveFontColor fntcol =
   case fntcol of
      Pink ->
         ELE.rgb 0.8 0.6 0.7
      CuteGreen ->
         ELE.rgb 0.5 0.9 0.7
      CuteBlue ->
         ELE.rgb 0.4 0.9 0.9


emph : FontColor -> String -> ELE.Element msg
emph fontCol str =
   ELE.el
      [ Font.color <| giveFontColor fontCol
      , Font.size 22
      ]
      ( ELE.text str )
