## Morphing a graph using Grid. 
Topology (vertices and edges) and name remain the same, display geometry changes. (That is position of the graph)
Vertices are reconstructed with the new positions
Edges are created between the new vertices according to the original edges
All together a new graph is created but it's topology is the same as the original one.
The new graph can be said to be visually morphed version of the original. But essentially the same graph connection wise and vertex name and
vertex color wise.

```Elm
morphGraph : Graph -> Grid -> Graph
morphGraph graph grid =
   let
      updatedVertices = List.map2 updatePositionVertex graph.vertices grid
      functionn = updateEdge updatedVertices
      updatedEdges = List.map functionn graph.edges
   in
      Graph updatedVertices updatedEdges
```

## Updating a Vertex with a new position
a vertex is generated with the same name colour but different position.

```Elm
updatePositionVertex : Vertex -> Vec3 -> Vertex
updatePositionVertex ver position =
   Vertex ver.name position ver.color
```


## Put the Edges back.
We are in a way creating a new graph, but it has the vertices of the same name, color but differnt positions
For the edges to be the same as before 
updateEdge takes an (old edge) and a (new list of vertices)
then produces a Edge with new vertices according to the old Edge

```Elm
updateEdge : List Vertex -> Edge -> Edge
updateEdge vs e =
      let v1 = e.vertexOne
          v2 = e.vertexTwo
      in case ((lookUpVertex v1.name vs), (lookUpVertex v2.name vs)) of
          (Nothing, _) -> Edge v1 v2
          (_, Nothing) -> Edge v1 v2
          (Just ver1, Just ver2) -> Edge ver1 ver2

lookUpVertex : Int -> List Vertex -> Maybe Vertex
lookUpVertex name vs =
   case vs of
      [] -> Nothing
      (x::xs) -> if name == x.name then Just x else lookUpVertex name xs
``` 
## Creation of Linear Grids
Grid is nothing but a list of 3d vectors Vec3

```Elm
makelinear : Int -> List Vec3
makelinear n = 
   let
      divider = toFloat (n-1)
   in
      List.range 0 (n-1) |> List.map (toFloat >> (\y -> y/divider)) |> List.map (\y -> vec3 0 y 0)
```
## Bipartite form of grid
### So that our project does not become graph problem solving
We have the answer to the isomorphism problem here

```Elm
setLeft : List Int
setLeft = [1,6,8,3] 
setRight : List Int
setRight = [5,2,4,7]
```

Here as set of numbers in the left and the right are being tupled with list of vertical vector grids
and then sorted according to index numbers
This gives node 1 its vector at position left of bipartite graph
This gives node 6 its vector at position left of bipartite graph
What it does is that the vector on the second position on the left grid goes to 6th on the final grid
Vector on the 3rd position of the left grid goes to the 8th on the final grid

```Elm
bipartiteGrid = 
   let
      leftTupled = List.map2 (\x y -> (x, y)) setLeft linearGridLeft 
      rightTupled = List.map2 (\x y -> (x, y)) setRight linearGridRight 
      totalGrid = leftTupled ++ rightTupled
   in
      List.map (\(x,y) -> y) (List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) totalGrid)
```
