## Topic Explanation

### Topics Chosen for the Project
- Graph Isomorphism
- Hamiltonian Cycle
- Graph Colouring
- Indentification of Independent Set
- Identification of Cliques in a Graph
- Finding Minimum spanning tree in Graph
- Finding of Vertex cover
- Max k-Cut
- Tree Width

### Graph Isomorphism:
Two graphs G1 and G2 are isomorphic if there is a one-one correspondence between the vertices of G1 and G2 
such that the number of edges between any two vertices in G1 is equal to the number of edges joining the corresponding 
vertices of G2. Here the graphs may appear to be different in appearance and the labeling of the nodes and edges. But the way one vertex is connected to another in one graph is same to another. Therefore given two graphs, detecting if the graphs are Isomorphic is a problem to solve.
One way to explain this would be to manipulate the position of vertices and edges to be appear same as it's isomorphic counterpart.
We want to show what isomorphism is.

Visualization:
- Display two graphs which are visibly (topologically) different but isomorphic.

User Interaction:
	- Given a initial graph which the user can manipulate by draging its vertices to make it look equivalent to it's isomorphic equivalent.
	- Position of vertices and edges in one of the graphs can be moved in an animation to make it look visually the same as
  	  its isomorphic counterpart. Graph drawing algorithms should be employed here.

### Hamiltonian graphs:
Graphs containing walks (moving from one edge to another) that include every vertex exactly once, ending at
initial vertex. (so we should start and end in same point without repeating vertices and cover all the vertices).

Visualization:
- Explaination can be given by giving an example graph with a hamiltonian cyle highlighted on it.
- We can also give negative examples of paths which are not hamiltonian.
- There can also be an Exercise for the user in which she is given a graph and and asked to mark the edges which would make a hamiltonian cycle.
- The program should check if that is a hamiltonian cylce by checking if all the related constraints defined in the definition 
  of hamiltonian cycle are satisfied which are if all the vertices are visited and also that no vertex has been visited twice. 

### Graph Clouring
It is an optimization problem, where the objective is to assign to vertices of a graph a colour such that no two adjacent vertices
have the same color, while keeping the number of colours employed to a minimum.

Visualization: 
- A properly coloured graph, which satisfies all the constraints can be displayed.
- Integer Linear Programing can be explained here as well.

User Interaction:
- User is allowed to color vertices.
- The program will warn the user if adjacent nodes are colored the same.
- Will be like solving sudoku.

### Identification of Maximum Independent Set
An Independent Set of a graph is a set of vertices such that no two vertices in that set are adjacent to each other.
A Maximum Independent set is an Inpendent set with the largest possible number of vertices.

Visualization:
- Show a graph with members of an Independent Set coloured differently from the rest of the vertices.
- Show the same graph with maximum independent set.

### Identification of Cliques in a graph
A clique is a set of vertices of a graph such that all the vertices are
connected to each other. This set is defined in such a way that there is no
other vertex in the graph which can be added to the set, while preserving the
property that all the vertices are connected to every other.
Visualization: 
- In a graph, a clique can be highlighted by colouring the vertices and the edges involed in the clique.

### Finding Minimum spanning tree in Graph
In a connected graph, minimum spanning tree is a subset of edges such that, all the vertices are connected. This should be a tree
without any cycles and the summation of weights should be minimum if there are more than one spanning trees present in the graph.

Visualization:
- In a succession the following can be done
	- Show what a connected graph is. (Define what a connected graph is.)
	- Draw spanning tree on the graph. (Define what a spanning tree is.)
	- Draw a minimum spannig tree. (Show that the constraints of the definition are satisfied.)

### Minimum Vertex Cover
Minimum Vertex cover of a graph is the minimum amount of vertices such that, all the edges in the graph 
must have one of such vertices as at least one of their endpoints.

Visualization:
Animation:
- Show how only a few vertices can cover all the edges, and such vertices are the vertex cover of the graph.
- Show all the constraints of the definition are satisfied.

Interactive:
- The user chooses an vertex and all the edges incident on it light up.
- Keeps choosing a vertex until all the edges light up.


### Max k-Cut
A maximum cut, is partioning the vertices of a graph in two groups such that the number of edges between these two
groups is maximum. In a weighted graph, where the edges are weighted, the weights of the edges are also taken into consideration.
A maximum k-cut, is generalised version of maximum cut, where the graph is partitioned into k subsets, such that the number of
edges between these groups is maximised.

Visualization:

- 2-cut Animation:
	- Making a 2-cut by a curved line on a simple graph to show how maximum number of edges have been cut between the two groups of vertices.
	  or classifying the vertices of the two sets by depicting them in two different graph.
	- Then clustering them and seperating the two groups by a distance  (of course without breaking any edges) to show the flow of edges between 
  the two groups.

- Bipartite Graph (as a trivial example):
	- Showing that a bipartite graph when it is recognised as a bipartite graph is already a trivial 2-cut graph.

- User Interaction:
	- Let the user choose the vertices he wants to be in set A. The rest of vertices will auto-matically will be assigned to set B. 
	  The program will show the number of edges which flow between the two sets.
	
Animation for k-cut:
	- Making a 3-cut by two curved lines on a simple graph to show how maximum number of edges have been cut by the two lines.
	- Making a 2-cut by a line on a weighted graph, where the cut is made keeping the weights of the edges into consideration.

- Ideas on constructing a graph such that max cut (2-cut) is known before hand.
	- Take two sets of vertices, A and B, initialy with zero edges in the graph. 
  	  Whenever an edge is made between the vertices of A, then two edges is made between vertices of set A and B.


### Tree Width
We will explain in two parts. First we will define what a tree decomposition of
a graph is. Then we will define tree width of the graph.

Formally, a tree decomposition of G = (V,E) consists of a tree T and a subset Vt <= V associated with each
node t <- T. We will call the subsets Vt pieces of tree decomposition. T and Vt must satisfy:
- (Node coverage) Every node of G belongs to at least one piece Vt.
- (Edge coverage) For every edge e of G, there is some piece Vt containing both ends of e.
- (Coherence) Let t1, t2 and t3 be three nodes of T such that t2 lies on the path from t1 to t3. 

All graphs have tree decompositions. A trivial tree decomposition of any graph has just one node with all
the vertices of the graph residing in it. It will satisfy all the conditions mentioned in the definition
of tree decomposition. But this trivial decomposition will not be useful as it will not
have seperable properties of a tree.

Therefore we must look for tree decompositions which have small pieces.
Tree width is defined as the size of the biggest Vt - 1.

Therefore tree width of G is the minimum width of any tree decomposition of G.
  https://ict.iitk.ac.in/wp-content/uploads/CS345-Algorithms-II-Algorithm-Design-by-Jon-Kleinberg-Eva-Tardos.pdf
