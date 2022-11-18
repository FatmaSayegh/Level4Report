# Colors
## To have different pallete of colour ranges

```Elm
type ColorRegion = First | Second | Third
```


We start with a list of integers [0 .. n] then converted them to float
Normalized the whole list to [0 .. 1.0]
Map Color.hsl accepting hue from the list [1 .. 0]
So finally we have a list of colors

```Elm
listOfColors : ColorRegion -> Int -> List Color
listOfColors region n = 
   let
      firstRegion = List.range 0 (n-1) |> List.map (toFloat) |> List.map (\x -> x / (3 * (toFloat (n-1))))
   in case region of
         First -> firstRegion |> List.map (\h -> Color.hsl h 1 0.7)
         Second -> List.map (\x -> x + 0.33) firstRegion |> List.map (\h -> Color.hsl h 1 0.5)
         Third -> List.map (\x -> x + 0.66) firstRegion |> List.map (\h -> Color.hsl h 1 0.5)
```
