module Main exposing (..)
import Element exposing (Element, el, rgb, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


main = view "Bonkers"

view dummy = 
   Element.layout []
   myElement

myElement : Element msg
myElement =
   el
      [ Background.color (rgb 0.8 0.8 0.8)
      , Border.color (rgb 0 0.7 0)
      ]
      (text "You've made a stylish element!")
