module FontSize exposing 
   ( getFontSize
   , FontSize(..)
   , FontColor(..)
   , DisplaySize
   , DeviceType(..)
   , giveFontColor
   , getDeviceType
   --, emph
   , emphForScreen
   )

import Element as ELE
import Element.Font as Font

type FontSize
   = Head
   | Normal
   | Emph
   | Title

type alias DisplaySize =
   { width : Int
   , height : Int
   , deviceType : DeviceType
   }

type DeviceType
   = XXL
   | XL
   | Large
   | Small
   | XS

getDeviceType : Int -> DeviceType
getDeviceType width =
         if width > 1800 then
            XXL
         else if width > 1500 then
            XL
         else if width > 1100 then
            Large
         else if width > 800 then
            Small
         else
            XS

getFontSize : FontSize -> DeviceType -> Int
getFontSize f deviceType =
   case f of
      Head ->
         case deviceType of
            XXL ->
               30
            XL ->
               25
            Large ->
               20
            Small ->
               18
            XS ->
               15
      Title ->
         case deviceType of
            XXL ->
               50
            XL ->
               45
            Large ->
               30
            Small ->
               30
            XS -> 
               25
      Emph ->
         case deviceType of
            XXL ->
               22
            XL ->
               19
            Large ->
               17
            Small ->
               14
            XS ->
               12
      Normal ->
         case deviceType of
            XXL ->
               18
            XL ->
               16
            Large ->
               14
            Small ->
               12
            XS ->
               10

type FontColor
   = Pink
   | CuteGreen
   | CuteBlue
   | Gold
   | Blue


giveFontColor : FontColor -> ELE.Color
giveFontColor fntcol =
   case fntcol of
      Pink ->
         ELE.rgb 0.8 0.6 0.7
      CuteGreen ->
         ELE.rgb 0.5 0.9 0.7
      CuteBlue ->
         ELE.rgb 0.4 0.9 0.9
      Gold ->
         ELE.rgb255 191 137 21
      Blue ->
         ELE.rgb 0.2 0.2 1


--emph : FontColor -> String -> ELE.Element msg
--emph =
--   emphForScreen XXL

emphForScreen : DeviceType -> FontColor -> String -> ELE.Element msg
emphForScreen deviceType fontCol str =
   ELE.el
      [ Font.color <| giveFontColor fontCol
      , Font.size (getFontSize Emph deviceType)
      ]
      ( ELE.text str )
