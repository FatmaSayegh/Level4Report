module Explanation exposing (..)

isomorphismExplanation =
   """
   Two graphs G1 and G2 are isomorphic if there is a one-one correspondence
   between the vertices of G1 and G2 such that the number of edges between any
   two vertices in G1 is equal to the number of edges joining the corresponding
   vertices of G2. Although the graphs may appear to be different in appearance and in
   the labeling of the nodes and edges. But the way one vertex is connected to
   another in one graph is same as another. 

   Animation

   The animation shown here takes a graph and changes the positions of the vertices
   without changing the edges which still connect the same vertices throughout the motion. 
   """
hamiltonianExplanation =
   """
   Graphs containing walks (moving from one edge to another) that include every
   vertex exactly once, ending at initial vertex. (so we should start and end
   in same point without repeating vertices and cover all the vertices).
   """


cliqueExplanation =
   """
   A clique is a set of vertices of a graph such that all the vertices are
   connected to each other. This set is defined in such a way that there is no
   other vertex in the graph which can be added to the set, while preserving the
   property that all the vertices are connected to every other.
   """
-- Just Like that!!!
--vert1 : Vertex
--vert1 = { name = "a"
--       , pos = {x = 100, y = 100}
--       }
--
--vert2 : Vertex
--vert2 = { name = "b"
--       , pos = {x = 300, y = 100}
--       }
--
--vert3 : Vertex
--vert3 = { name = "c"
--       , pos = {x = 200, y = 273}
--       }
--
--vert4 : Vertex
--vert4 = { name = "d"
--       , pos = {x = 400, y = 400}
--       }
--
--edge1 : Edge
--edge1 = { vertexOne = vert1
--        , vertexTwo = vert2
--        }
--
--edge2 : Edge
--edge2 = { vertexOne = vert1
--        , vertexTwo = vert3
--        }
--
--edge3 : Edge
--edge3 = { vertexOne = vert2
--        , vertexTwo = vert3
--        }
--
--graph1 : Graph
--graph1 =
--   {
--      vertices = [vert1, vert2, vert3]
--   ,  edges = [edge1, edge2, edge3]
--   }
