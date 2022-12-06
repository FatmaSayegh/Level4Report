module Explanation exposing (..)

isomorphismExplanation =
   """
   Two graphs G1 and G2 are isomorphic if there is a one-one correspondence
   between the vertices of G1 and G2 such that the number of edges between any
   two vertices in G1 is equal to the number of edges joining the corresponding
   vertices of G2. 
   Although the graphs may appear to be different in appearance and in
   the labeling of the nodes and edges. But the way one vertex is connected to
   another in one graph is same as another. 

   The animation shown here takes a graph and changes the positions of the
   vertices without changing the edges which still connect the same vertices
   throughout the motion. In the animation if there is an edge between any two
   vertices in a graph, then there is a edge between the corresponding vertices
   in the other graphs as well.
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

maxCutExplanation =
   """
   A maximum cut, is partioning the vertices of a graph in two groups such that the number of edges between these two
   groups is maximum.
   """


coloringExplanation =
   """
   The graph coloring problems objective is to assign colors to the vertices of a graph such that no to adjacent
   vertices have the same color such that the number of colors utilized are kept at minimum.
   The graph shown in the picture, needs three colors to color it properly.
   """

howToColor =
   """
   Choose a color from the color palette by clicking on one of the colors and apply it to one of the vertices.
   While coloring the graph make sure that no two adjacent vertices are colored the same. For if they are, the
   edges connecting them will be displayed differently to let you know of the mistake.
   """

vertexCoverExplanation =
   """
   Minimum Vertex cover of a graph is the minimum amount of vertices such that,
   all the edges in the graph must have one of such vertices as at least one of
   their endpoints.
   """
