Terms and Definitions
=====================

The following are the basic terms and definitions which are used in
Graph theory.

::: {#some-basic-definitions}
Some basic definitions:
-----------------------
:::

-   Vertices: A graph has vertices. Which can be understood as points.

-   Edge: An edge is an unordered pair of vertices.

-   V(G) is the vetex set of graph G.

-   E(G) is the edge set of graph G.

-   Degree of a vertex: deg(v). Number of edges connected to a vertex.

-   isolated vertex v: deg(v) = 0

-   End vertex w: deg(w) = 1

::: {#some-basic-facts}
Some basic facts
----------------
:::

::: {#handshaking-lemma}
### Handshaking Lemma
:::

In any graph the sum of all the the vertex-degree is an even number - in
fact, twice the number of edges, since each edge contributes exactly 2
to the sum. Total summation of is edes\*2

::: {#adjacency}
### Adjacency
:::

Two vertices are adjacent if they have an edge between them. Two edges
are adjacent if they have a vertex in common.

::: {#what-graphs-are-not-about}
### What graphs are not about
:::

metrical properties: length and shape of the edges. (Irrelevant in graph
theory). (so if an edge is smaller , larger etc doesnt matter what
matters if the edges between two vertices are connected or not )

::: {#some-more-definitions}
### Some more definitions
:::

::: {#simple-graphs}
#### Simple graphs:
:::

graphs with no loops and multiple edges.

::: {#general-graph}
#### General graph:
:::

loops and multiple edges are allowed.

::: {#digraphs}
#### Digraphs:
:::

Directed graphs. When edges have arrows.

::: {#walk}
#### Walk:
:::

A way of getting from one vertex to another, and consists of a sequence
of edges. P -\> Q -\> R is a walk of length 2

::: {#path}
#### Path:
:::

A walk in which no vertex appears more than once.

::: {#cycle}
#### Cycle
:::

A path like this: Q -\> S -\> T -\> Q is called a cycle.

::: {#subgraph}
#### Subgraph
:::

G - e is the graph obtained from G by deleting the edge e.

::: {#adjecency-matrix}
#### Adjecency Matrix
:::

If G is a graph with vertices labelled {1,2,..n}, its adjacency matrix A
is n \* n matrix whose ijth entry is the number of edges joining vertex
i and vertex j.

::: {#null-graph}
#### Null Graph
:::

Edge set is empty. All vertices are isolated.

::: {#complete-graph}
#### Complete Graph
:::

A simple graph in which each pair of distinct vertices are adjacent is a
complete. Complete Graph of n vertices are denoted as Kn. They have
n(n-1)/2 edges.

::: {#cycle-graphs}
#### Cycle Graphs
:::

A connected graph which is regular of degree 2. A cycle graph of n
vertices is denoted by Cn.

::: {#path-graph}
#### Path Graph
:::

A graph obtained from Cn by removing an edge. Denoted by Pn.

::: {#wheel}
#### Wheel
:::

The graph obtained from Cn-1 by joining each vertex to a new vertex v is
wheel on n vertices, denoted by Wn.

::: {#regular-graph}
#### Regular Graph
:::

Each vertex has the same degree. With that degree r, the graph is a
regular of degree r of r-regular.

::: {#bipartite-graph}
#### Bipartite Graph
:::

If the vertex set of a graph G can be split into two disjoint sets A and
B such that each edge of G joins a vertex of A and a vertex of B, then G
is a bipartite graph. (like when we divide it into two sets anything in
a will connect with set b not another set of a)

::: {#hamiltonian-graphs}
#### Hamiltonian graphs
:::

Graphs containing walks that include every vertex exactly once, ending
at initial vertex. ( so we should start and end in same point without
repeating vertices not all vertices have to be used )
