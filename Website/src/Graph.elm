module Graph exposing (..)

import Math.Matrix4 as M4 exposing (..)
import Math.Vector3 exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)
import Point2d as Pt
import LineSegment2d as Ln
import Length as Len
import Color exposing (Color)
import Messages exposing (Msg(..))

type alias Vertex =
    { name : Int, pos : Vec3, color : Color, glow : Bool }


type alias Edge =
    { vertexOne : Vertex, vertexTwo : Vertex }


type alias Graph =
    { vertices : List Vertex, edges : List Edge }


-- Grid.
-- A grid is a list of positions. This type is used to provide positions for vertices
-- for graph formation or a geometry a graph can morph into by situating it's vertices on
-- the positions contained in the grid.

type alias Size = Int

type alias Grid =
    List Vec3

type alias ShapeTransition =
    { graphA : Graph -- Will remain static
    , graphB : Graph  -- Will move towards final Grid when animationOn is True
    , finalGrid : Grid 
    , animationOn : Bool
    , specialToken : Token
    }

type Token =
   MakeKCut
   | NoToken

-- Making Graphs
-- Make Polygonal Graphs.
-- Symplifying the creation of makeing polygonal graphs.
-- makeGraph takes a Gtype, position of the graph, size of the graph (radius of
-- the circumcribing circle), and inital orientation of the first vertex.

-- Polygonal Graph Types
-- Type created to aid creation of simple shapes
-- used by makeGrah function.
-- Even though a Graph can be created just by invoking the data constructor
-- Graph, without the need of Gtype or makeGraph function.
-- A graphs shape can be Polygon Cycle, in which it is created as cyclical graph with vertices
-- taking the place of vertices of a geometric polygon.
-- PolygonFullyConnected is the same as the previous one, but it is a fully connected graph. i.e. all the vertices
-- of this graph is connected to each other
-- PolygonCycleDoll will create polygonal Russian dolls with cycles and spokes when given the number of
-- vertices of the outer or inner doll. They have the same number of vertices anyway.
-- each vertex is named Ints for simplicity.

type Gtype
    = PolygonCycle Int
    | PolygonFullyConnected Int
    | PolygonCycleDoll Int

makeGraph : Gtype -> Vec3 -> Vec3 -> Float -> Graph
makeGraph graphType position size initialAngle =
    case graphType of
        PolygonCycle n ->
            let
                vertices =
                    List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
            in
            Graph vertices (List.map2 Edge vertices (shiftListCycle vertices))

        PolygonFullyConnected n ->
            let
                vertices =
                    List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)
            in
            Graph vertices (fullyConnectVertices vertices)

        PolygonCycleDoll n ->
            let
                verticesSetA =
                    List.map3 (\name g c -> Vertex name g c False) (List.range 1 n) (parametricPolygon n size position initialAngle) (listOfColors First n)

                verticesSetB =
                    List.map3 (\name g c -> Vertex name g c False)
                        (List.range (n + 1) (2 * n))
                        (parametricPolygon n (Math.Vector3.scale 0.5 size) position initialAngle)
                        (listOfColors Second n)

                allVertices =
                    verticesSetA ++ verticesSetB

                edgesCycleSetA =
                    List.map2 Edge verticesSetA (shiftListCycle verticesSetA)

                edgesCycleSetB =
                    List.map2 Edge verticesSetB (shiftListCycle verticesSetB)

                spokesSetASetB =
                    List.map2 Edge verticesSetA verticesSetB
            in
            Graph allVertices (edgesCycleSetA ++ edgesCycleSetB ++ spokesSetASetB)



makeEdgesWithTuples : List (Int, Int) -> List Vertex -> List Edge
makeEdgesWithTuples tuples vertices =
   case tuples of
      [] ->
         []
      (tu::tus) ->
         case (makeEdgeWithTuple tu vertices) of 
            Nothing ->
               makeEdgesWithTuples tus vertices
            (Just edge) ->
               edge :: makeEdgesWithTuples tus vertices



makeEdgeWithTuple : (Int, Int) -> List Vertex -> Maybe Edge
makeEdgeWithTuple tu vs =
   case tu of
      (name1, name2) ->
         case (lookUpVertex name1 vs, lookUpVertex name2 vs) of
            (Nothing, _ ) ->
               Nothing
            ( _, Nothing ) ->
               Nothing
            (Just vertexOne, Just vertexTwo) ->
               Just (Edge vertexOne vertexTwo)

executeShapeTransition : ShapeTransition -> ShapeTransition
executeShapeTransition shapeTransition =
   if (distanceBetweenGraphAndGrid shapeTransition.graphB shapeTransition.finalGrid < 10)
   then { shapeTransition 
            | animationOn = False
            , graphB = morphGraph shapeTransition.graphB shapeTransition.finalGrid
        }
   else { shapeTransition 
            | graphB = moveTowards shapeTransition.graphB shapeTransition.finalGrid
        }


distanceBetweenGraphAndGrid : Graph -> Grid -> Float
distanceBetweenGraphAndGrid graph grid =
   let
      listOfDistances =
         List.map2 (\ver pos -> distance pos ver.pos) graph.vertices grid
   in
      List.sum listOfDistances
changeColorOfVertex : Int -> Color -> Graph -> Graph
changeColorOfVertex name color graph =
   let 
      newVertices =
         List.map (\v ->
                     if v.name == name
                     then
                        {v | color = color } 
                     else
                        v)
                  graph.vertices

      createEdge =
         updateEdge newVertices
   in
   Graph newVertices (List.map createEdge graph.edges)

-- Change the glow status of a Vertex
-- This function changes the glow status of the Vertex to True or False
-- depending on the input status given.
changeGlowVertex : Bool -> Int -> Graph -> Graph
changeGlowVertex status name graph =
    let
        new_vertices vs =
            case vs of
                [] ->
                    []

                x :: xs ->
                    (if x.name == name then
                        { x | glow = status }

                     else
                        x
                    )
                        :: new_vertices xs
    in
    Graph (new_vertices graph.vertices) graph.edges

makeUnglowAllVerticesBut : Int -> Graph -> Graph
makeUnglowAllVerticesBut name graph =
    let
        new_vertices vs =
            case vs of
                [] ->
                    []

                x :: xs ->
                    (if x.name == name then
                        x
                     else
                        { x | glow = False}
                    )
                        :: new_vertices xs
    in
    Graph (new_vertices graph.vertices) graph.edges

makeUnglowAllVertices : Graph -> Graph
makeUnglowAllVertices graph =
    let
        new_vertices vs =
            case vs of
                [] ->
                    []

                x :: xs ->
                        { x | glow = False} :: new_vertices xs
    in
    Graph (new_vertices graph.vertices) graph.edges

toggleGlowVertex : Int -> Graph -> Graph
toggleGlowVertex  name graph =
    let
        new_vertices vs =
            case vs of
                [] ->
                    []

                x :: xs ->
                    (if x.name == name then
                        { x | glow = not (x.glow) }

                     else
                        x
                    )
                        :: new_vertices xs
    in
    Graph (new_vertices graph.vertices) graph.edges

-- Calculating Grid Points for Kinematics.
-- This function takes in a graph, a grid (set of 3d vectors) and creates a grid which is
-- in between the graph and the final destination.


calcNextGrid : Graph -> Grid -> Float -> Grid
calcNextGrid graph grid time =
    List.map2 (advanceVertexTowardsPosition time) graph.vertices grid



-- Calculation of Intermediate Vector Grid Point.
-- First a vector between inital Vertex and final grid point is calculated.
-- Then it is scaled down by the number of steps.
-- Finally it is added to the position vector of the the vertex.


advanceVertexTowardsPosition : Float -> Vertex -> Vec3 -> Vec3
advanceVertexTowardsPosition time vertex position =
    let
        dif =
            sub position vertex.pos
    in
      Math.Vector3.add vertex.pos (Math.Vector3.scale (1 / time) dif)



-- Drawing
drawVertex : Vertex -> S.Svg Msg
drawVertex v =
    circle 10 v.pos v.color v.name


drawGoldenCircle : Vertex -> S.Svg Msg
drawGoldenCircle v =
    ccircle 15 v.pos "#BF8915" v.name


drawSelectedVertex : Vertex -> S.Svg Msg
drawSelectedVertex  v =
    ccircle 13 v.pos "#BF8915" v.name

writeVertexName : Vertex -> S.Svg Msg
writeVertexName v =
    writeName v.name v.pos


drawEdge : Edge -> S.Svg msg
drawEdge e =
    line e.vertexOne.pos e.vertexTwo.pos


drawSpecialEdge : Edge -> S.Svg msg
drawSpecialEdge e =
    lline e.vertexOne.pos e.vertexTwo.pos




-- Basic Geometry and Linear Algebra
-- Basic Geometry and Linear Algebra
-- Basic Geometry and Linear Algebra
-- Basic Geometry and Linear Algebra



-- situateShape which was used to scale and locate
-- the miniturised version of the linear set of vertices
linearGrid n position size =
    situateShape position size (makelinear n)



-- Position of vertices of a polygon related to origin.
-- Create a list of floats and make angles out of it.
--


-- rotate a vector by angle a in the x-y plane
rotateVector v a =
    let
        rotation =
            M4.rotate a (vec3 0 0 1) M4.identity
    in
    M4.transform rotation v

makePolygon : Float -> Int -> List Vec3
makePolygon startAngle n =
    let
        increment =
            2 * pi / toFloat n

        initialVector =
            vec3 1 0 0

        angles =
            List.range 0 (n - 1) |> List.map ((+) startAngle << (*) increment << toFloat)
    in
    List.map (rotateVector initialVector) angles


makelinearIn2D : Int -> Int -> List Vec3
makelinearIn2D n m =
    let
        divider =
            toFloat ((Basics.max n m) - 1)

        scalar =
            (1/divider)

        xRange =
            List.range 0 (m - 1)
            |> List.map (toFloat)
    in
    List.range 0 (n - 1) 
      |> List.map toFloat
      |> List.map (List.repeat m)
      |> List.map (List.map2 (\x y -> (x,y)) xRange)
      |> List.concat
      |> List.map (\(x,y)-> vec3 x y 0)
      |> List.map (\v -> Math.Vector3.scale scalar v)

--takes a centre , and places the shape there
--takes a scaling vector, and expands according to that
--traslateTrans is a matrix which translates a vec to a center.
--scaleTrans is a matrix which magnifies vec
makelinear : Int -> List Vec3
makelinear n =
    let
        divider =
            toFloat (n - 1)
    in
    List.range 0 (n - 1) |> List.map (toFloat >> (\y -> y / divider)) |> List.map (\y -> vec3 0 y 0)


situateShape : Vec3 -> Vec3 -> List Vec3 -> List Vec3
situateShape position scaleVec polygon =
    let
        translateTrans =
            M4.translate position M4.identity

        scaleTrans =
            M4.scale scaleVec M4.identity
    in
    List.map (M4.transform translateTrans << M4.transform scaleTrans) polygon


parametricPolygon : Int -> Vec3 -> Vec3 -> Float -> List Vec3
parametricPolygon n scaleVec position startAngle =
    situateShape position scaleVec <| makePolygon startAngle n

moveTowards : Graph -> Grid -> Graph
moveTowards graph grid =
    let
        intermediateGrid =
            calcNextGrid graph grid 100
    in
    morphGraph graph intermediateGrid


-- Scalable vector graphics
displaySvg elements =
    S.svg
        --[ SA.width "100%"
        --, SA.height "auto"
        [ SA.viewBox "0 0 400 400"
        ]
        elements

--## Morphing a graph using Grid.
-- This function changes display geometry of a graph according to an input grid (list of vector positions).
-- while preserving the topology.
-- Vertices are reconstructed with the new positions. Edges are created between
-- the new vertices according to the incidence of original edges.


morphGraph : Graph -> Grid -> Graph
morphGraph graph grid =
    let
        updatedVertices =
            List.map2 updatePositionVertex graph.vertices grid

        createEdge =
            updateEdge updatedVertices

        updatedEdges =
            List.map createEdge graph.edges
    in
    Graph updatedVertices updatedEdges



-- a vertex is generated with the same name colour and glow but different position.


updatePositionVertex : Vertex -> Vec3 -> Vertex
updatePositionVertex ver position =
    Vertex ver.name position ver.color ver.glow



--## Put the Edges back.
--We are in a way creating a new graph, but it has the vertices of the same name, color but differnt positions
--For the edges to be the same as before
--updateEdge takes an (old edge) and a (new list of vertices)
--then produces a Edge with new vertices according to the old Edge


updateEdge : List Vertex -> Edge -> Edge
updateEdge vs e =
    let
        v1 =
            e.vertexOne

        v2 =
            e.vertexTwo
    in
    case ( lookUpVertex v1.name vs, lookUpVertex v2.name vs ) of
        ( Nothing, _ ) ->
            Edge v1 v2

        ( _, Nothing ) ->
            Edge v1 v2

        ( Just ver1, Just ver2 ) ->
            Edge ver1 ver2


lookUpVertex : Int -> List Vertex -> Maybe Vertex
lookUpVertex name vs =
    case vs of
        [] ->
            Nothing

        x :: xs ->
            if name == x.name then
                Just x

            else
                lookUpVertex name xs




-- Will connect 1 to 3,4,5,6
-- Then in next call 2 ot 3,4,5,6
-- and so on recursively


fullyConnectVertices : List Vertex -> List Edge
fullyConnectVertices vs =
    case vs of
        [] ->
            []

        x :: [] ->
            []

        x :: xs ->
            List.map (Edge x) xs ++ fullyConnectVertices xs


linearConnectVertices : List Vertex -> List Edge
linearConnectVertices vs =
    case vs of
        [] ->
            []

        [ x ] ->
            []

        x :: y :: xs ->
            Edge x y :: linearConnectVertices (y :: xs)



-- The head becomes the last element
-- and the second element becomes head


shiftListCycle xs =
    case List.tail xs of
        Just ys ->
            case List.head xs of
                Just h ->
                    ys ++ [ h ]

                Nothing ->
                    []

        Nothing ->
            []

-- To have different pallete of colour ranges


type ColorRegion
    = First
    | Second
    | Third



-- We start with a list of integers [0 .. n] then converted them to float
-- Normalized the whole list to [0 .. 1.0]
-- Map Color.hsl accepting hue from the list [1 .. 0]
-- So finally we have a list of colors


listOfColors : ColorRegion -> Int -> List Color
listOfColors region n =
    let
        firstRegion =
            List.range 0 (n - 1) |> List.map toFloat |> List.map (\x -> x / (3 * toFloat (n - 1)))
    in
    case region of
        First ->
            firstRegion |> List.map (\h -> Color.hsl h 1 0.7)

        Second ->
            List.map (\x -> x + 0.33) firstRegion |> List.map (\h -> Color.hsl h 1 0.5)

        Third ->
            List.map (\x -> x + 0.66) firstRegion |> List.map (\h -> Color.hsl h 1 0.5)


circle : Size -> Vec3 -> Color -> Int -> S.Svg Msg
circle size pos color name =
    S.circle
        [ SA.cx (String.fromInt <| round <| getX pos)
        , SA.cy (String.fromInt <| round <| getY pos)
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ Color.toCssString color ++ ";")
        , SE.onMouseOver (HoverOver name)
        , SE.onMouseOut (MouseOut name)
        , SE.onClick (VertexClicked name)
        ]
        []


ccircle : Size -> Vec3 -> String -> Int -> S.Svg Msg
ccircle size pos color name =
    S.circle
        [ SA.cx (String.fromInt <| round <| getX pos)
        , SA.cy (String.fromInt <| round <| getY pos)
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ color ++ ";")
        , SE.onMouseOver (HoverOver name)
        , SE.onMouseOut (MouseOut name)
        , SE.onClick (ToggleVertexStatus name)
        ]
        []

drawIntersectionPoint : Size -> Vec3 -> S.Svg Msg
drawIntersectionPoint size pos =
    S.circle
        [ SA.cx (String.fromInt <| round <| getX pos)
        , SA.cy (String.fromInt <| round <| getY pos)
        , SA.r (String.fromInt size)
        , SA.style ("fill: " ++ "blue" ++ ";")
        ]
        []


writeName : Int -> Vec3 -> S.Svg Msg
writeName name pos =
    S.text_
        [ SA.x (String.fromInt <| round <| getX pos)
        , SA.y (String.fromInt <| round <| getY pos)
        , SA.class "small"
        , SA.fontSize "7px"
        , SA.textAnchor "middle"
        , SE.onClick (VertexClicked name)

        -- , SA.stroke "dark grey"
        ]
        [ S.text (String.fromInt name) ]



-- takes 2 positions and draw a line


line : Vec3 -> Vec3 -> S.Svg msg
line veca vecb =
    S.line
        [ SA.x1 (String.fromInt <| round <| getX veca)
        , SA.y1 (String.fromInt <| round <| getY veca)
        , SA.x2 (String.fromInt <| round <| getX vecb)
        , SA.y2 (String.fromInt <| round <| getY vecb)
        , SA.stroke "white"
        ]
        []


lline : Vec3 -> Vec3 -> S.Svg msg
lline veca vecb =
    S.line
        [ SA.x1 (String.fromInt <| round <| getX veca)
        , SA.y1 (String.fromInt <| round <| getY veca)
        , SA.x2 (String.fromInt <| round <| getX vecb)
        , SA.y2 (String.fromInt <| round <| getY vecb)
        , SA.stroke "#BF8915"
        , SA.strokeWidth "3"
        ]
        []

drawIntersectionPoints points =
   List.map (drawIntersectionPoint 3) points

getStringFromVertices : List Vertex -> String
getStringFromVertices vs =
    case vs of
        [] ->
            ""

        x :: [] ->
            String.fromInt x.name

        x :: [ y ] ->
            String.fromInt x.name ++ " and " ++ getStringFromVertices [ y ]

        x :: xs ->
            String.fromInt x.name ++ ", " ++ getStringFromVertices xs

findIntersection : (Vec3, Vec3) -> (Vec3, Vec3) -> Maybe Vec3
findIntersection lineOne lineTwo =
   let
      (p11, p12) = 
         lineOne
      point1 =
         Pt.meters (getX p11) (getY p11)
      point2 =
         Pt.meters (getX p12) (getY p12)
      line1 =
         Ln.fromEndpoints (point1, point2)

      (p21, p22) =
         lineTwo
      point3 =
         Pt.meters (getX p21) (getY p21)
      point4 =
         Pt.meters (getX p22) (getY p22)
      line2 =
         Ln.fromEndpoints (point3, point4)

   in
   case (Ln.intersectionPoint line1 line2) of
      Nothing ->
         Nothing
      Just poinIn ->
           let 
               x = poinIn |> Pt.xCoordinate |> Len.inMeters

               y = poinIn |> Pt.yCoordinate |> Len.inMeters
           in
           Just (vec3 x y 0)

findEdgeLines : List Edge -> List (Vec3, Vec3)
findEdgeLines edges =
   case edges of
      (x::xs) ->
         (x.vertexOne.pos, x.vertexTwo.pos) :: findEdgeLines xs
      [] ->
         []

getHaloVertices : Graph -> List Edge -> List Vertex
getHaloVertices g es =
   let
      glowingVertices = List.filter (\ver -> ver.glow) g.vertices
   in
      List.filter (\v -> not (isVertexInList v glowingVertices)) <| List.filter (\v -> isVertexInEdges v es) g.vertices

isVertexInList : Vertex -> List Vertex -> Bool
isVertexInList v vs =
   case (lookUpVertex v.name vs) of
      Just _ ->
         True
      Nothing ->
         False


isVertexInEdges : Vertex -> List Edge -> Bool
isVertexInEdges v es =
    case es of
        [] ->
            False

        x :: xs ->
            if v.name == x.vertexOne.name || v.name == x.vertexTwo.name then
                True

            else
                isVertexInEdges v xs


seperateEdges : Graph -> ( List Edge, List Edge )
seperateEdges g =
    let
        specialVertices =
            List.filter (\ver -> ver.glow) g.vertices

        specialEdges =
            List.filter (\edge -> isEdgeIn edge specialVertices) g.edges

        normalEdges =
            List.filter (\edge -> not (isEdgeIn edge specialVertices)) g.edges
    in
    ( specialEdges, normalEdges )


isEdgeIn : Edge -> List Vertex -> Bool
isEdgeIn e vs =
    let
        v1 =
            e.vertexOne

        v2 =
            e.vertexTwo
    in
    case ( lookUpVertex v1.name vs, lookUpVertex v2.name vs ) of
        ( Nothing, Nothing ) ->
            False

        ( _, _ ) ->
            True

findPositionsOfTuples : (List Vertex) -> (Int, Int) -> Maybe (Vec3, Vec3)
findPositionsOfTuples vs tu =
   case tu of
      (name1, name2) ->
         case (lookUpVertex name1 vs, lookUpVertex name2 vs) of
            (Nothing, _ ) ->
               Nothing
            ( _, Nothing ) ->
               Nothing
            (Just vertexOne, Just vertexTwo) ->
               let
                  pullDown = vec3 0 20 0
                  pos1 = vertexOne.pos
                  pos2 = vertexTwo.pos
                  diff = normalize <| sub pos2 pos1
                  linePos2 = add pos2 (Math.Vector3.scale 50 diff)
                  linePos1 = sub pos1 (Math.Vector3.scale 50 diff)
                  (finalPos1, finalPos2) =
                     if (List.member vertexOne.name [1,2,3])
                        then
                           (  sub linePos1 pullDown
                           ,  sub linePos2 pullDown
                           )
                        else
                           (  add linePos1 <| Math.Vector3.scale 1.6 pullDown
                           ,  add linePos2 <| Math.Vector3.scale 1.6 pullDown
                           )
                           
               in
               Just (finalPos1, finalPos2)
