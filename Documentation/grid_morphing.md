## Morphing a graph using Grid. 
Topology (vertices and edges) and name remain the same, display geometry changes. (That is position of the graph)
Vertices are reconstructed with the new positions
Edges are created between the new vertices according to the original edges
All together a new graph is created but it's topology is the same as the original one.
The new graph can be said to be visually morphed version of the original. But essentially the same graph connection wise and vertex name and
vertex color wise.

```
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

```
updatePositionVertex : Vertex -> Vec3 -> Vertex
updatePositionVertex ver position =
   Vertex ver.name position ver.color
```


## Put the Edges back.
We are in a way creating a new graph, but it has the vertices of the same name, color but differnt positions
For the edges to be the same as before 
updateEdge takes an (old edge) and a (new list of vertices)
then produces a Edge with new vertices according to the old Edge

```
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

```
makelinear : Int -> List Vec3
makelinear n = 
   let
      divider = toFloat (n-1)
   in
      List.range 0 (n-1) |> List.map (toFloat >> (\y -> y/divider)) |> List.map (\y -> vec3 0 y 0)
```
