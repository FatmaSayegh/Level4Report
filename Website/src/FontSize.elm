module FontSize exposing (getFontSize, FontSize(..))

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
