colorDisplay : ColorDisplay
colorDisplay = 
   let
      initialGraph =
         makeGraph (PolygonCycleDoll 5) (vec3 200 100 0) (vec3 80 80 0) (pi /4)
         --makeGraph (PolygonCycleDoll 5) (vec3 200 130 0) (vec3 80 80 0) (pi /4)
      whiteVertices = 
         List.map (\v ->
            {v | color = (Color.rgb 1 1 1)})
            initialGraph.vertices
      createEdge =
         updateEdge whiteVertices
      newGraph = Graph whiteVertices (List.map createEdge initialGraph.edges)
   in
      ColorDisplay newGraph (Color.rgb 1 1 1) (Color.rgb 1 1 1)

colorPallete : ColorDisplay -> List (S.Svg Msg)
colorPallete display=
   let
      sizeBig = (vec3 20 35 0)
      sizeSmall = (vec3 20 20 0)
      sizeOfColor color = if display.chosenColor == color
                then sizeBig
                else sizeSmall
      red = (Color.rgb 1 0 0)
      green = (Color.rgb 0 1 0)
      blue = (Color.rgb 0 0 1)
      squareRed = makeSquare (vec3 170 230 0) (sizeOfColor red) red
      squareGreen = makeSquare (vec3 200 230 0) (sizeOfColor green) green 
      squareBlue = makeSquare (vec3 230 230 0) (sizeOfColor blue) blue 
   in
   [squareRed, squareGreen, squareBlue]

makeSquare : Vec3 -> Vec3 -> Color -> S.Svg Msg
makeSquare pos size color =
   S.rect
      [ SA.x (String.fromInt <| round <| getX pos)
      , SA.y (String.fromInt <| round <| getY pos)
      , SA.width (String.fromInt <| round <| getX size)
      , SA.height (String.fromInt <| round <| getY size)
      , SA.style ("fill: " ++ Color.toCssString color ++ ";")
      , SE.onClick (ColoringSelectColor color)
      ]
      []

paneThree display =
   displaySvg ((drawGraphForColoring display.graphA) ++ (colorPallete display))

explanationColoring : ColorDisplay -> Bool -> ELE.Element Msg
explanationColoring colorDisp helpStatus =
    let
      verticesOfSameColor edge =
         edge.vertexOne.color == edge.vertexTwo.color 
                              && edge.vertexOne.color /= (Color.rgb 1 1 1)

      miscoloredEdges = List.filter 
                           (\e -> verticesOfSameColor e) colorDisp.graphA.edges 
      coloredVertices = List.filter  
                              (\v -> v.color /= Color.rgb 1 1 1 ) 
                              colorDisp.graphA.vertices

    in
    ELE.column
         [ Font.color (ELE.rgb 1 1 1)
         --, ELE.height ELE.fill
         , ELE.spacing 20
         --, ELE.padding 40
         , ELE.height ELE.fill
         , ELE.width ELE.fill
         , Background.color <| ELE.rgb 0.2 0.2 0.2
         ]
         <|
         [  ELE.el
               [Font.size 30, Font.heavy] 
               (ELE.text "Graph Coloring")
         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text coloringExplanation ]

         ,  ELE.paragraph
               [ ELE.spacing 8 ] 
               [ ELE.text howToColor ]
         , ELE.paragraph
               []
               [ ELE.text 
                     """
                     As a challenge you may try to color
                     the graph with only two colors and see if
                     it is feasible.
                     """
               ]
        , Input.button
            [
              ELE.centerX
            ] 
            { onPress = Just VertexNonColor
            , label = Icons.rollbackOutlined [ Ant.width 70, Ant.height 50 ]
            }

        , ELE.paragraph
               []
               [ ELE.text <| if List.isEmpty coloredVertices
                                then
                                   ""
                                else
                                   """
                                   Coloring has started.
                                   """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if List.length coloredVertices > 1 && List.length miscoloredEdges == 0
                                then
                                   "Good going! Adjacent Vertices are colored differently."
                                else
                                   """
                                   """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if List.isEmpty miscoloredEdges
                                then
                                    ""
                                else
                                    String.join " "  <| List.map miscolorText miscoloredEdges
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if List.isEmpty miscoloredEdges
                                then
                                    ""
                                else
                                   """
                                   Try another color combination.
                                   Remember the rule; No two adjacent
                                   vertices must have the same color!
                                   """
               ]

        , ELE.paragraph
               []
               [ ELE.text <| if (List.isEmpty miscoloredEdges
                                 && List.length coloredVertices 
                                    == List.length colorDisp.graphA.vertices)
                                then
                                   """
                                   Congratulations! Graph has been colored fully and correctly.
                                   i.e. No two adjacent vertices have the same color.
                                   """
                                else
                                   ""
              ]
         , (if helpStatus == True then (helpParagraph GraphColoringHelp) else ELE.none)
         , lowerNavigation "Max Cut" "Vertex Cover"
         ]



miscolorText : Edge -> String
miscolorText e =
   "Vertex " ++ (String.fromInt e.vertexOne.name) 
             ++ " and vertex "
             ++ (String.fromInt e.vertexTwo.name)
             ++ " which are adjacent to each other are colored with the same color."

goColor : ColorDisplay -> Msg -> Model
goColor display msg =
   case msg of
      ColoringSelectColor color ->
         let
            newDisplay = 
               {display |
                  chosenColor = color
               }
         in
         GraphColoring newDisplay

      VertexClicked name ->
         let
            newGraph = changeColorOfVertex name display.chosenColor display.graphA 
            newDisplay = {display |
                              graphA = newGraph }
         in
         GraphColoring newDisplay

      VertexNonColor ->
         let
            whiteVertices = 
               List.map (\v ->
                  {v | color = (Color.rgb 1 1 1)})
                  display.graphA.vertices

            createEdge =
               updateEdge whiteVertices

            newGraph = Graph whiteVertices (List.map createEdge display.graphA.edges)

            newDisplay = {display |
                              graphA = newGraph }
         in
         GraphColoring newDisplay

      _ ->
         GraphColoring display

type alias ColorDisplay =
   { graphA : Graph
   , chosenColor : Color
   , defaultColor : Color
   }

drawGraphForColoring g =
    let
      verticesOfSameColor edge =
         edge.vertexOne.color == edge.vertexTwo.color && edge.vertexOne.color /= (Color.rgb 1 1 1)

      normalEdges = List.filter (\e -> not (verticesOfSameColor e)) g.edges 

      miscoloredEdges = List.filter (\e -> verticesOfSameColor e) g.edges 
    in
    List.map drawEdge normalEdges
        ++ List.map drawSpecialEdge miscoloredEdges
        ++ List.map drawVertex g.vertices
        ++ List.map writeVertexName g.vertices
