## Week 1
- Got to know the topic and the professor.

## Week 2
- Read the basics of graph terminologies and definitions.
- Made a tentative list of topics.
- Wrote the defintion of Graph Isomorphism and Hamiltonian paths. 

## Week 3 (Planned)
### Agenda given by the professor
- Wire frame, more reading, methodology

## Week 4
- Add two more things
	- Tree Width (Even she has problem understanding)
	- Max k-Cut
- Methodology about the functionality of the app. (At least write)
- The survey of hard problems.
- Asking from people, what are the hard problems and how they would want to visualize it given certain options.
- Two surveys - What are the hard problems? How would they want it to be visualized?
- Make a list of problems. Make a list of visualization techniques.
- Create Latex framework (Done)
- Create BibTex. (With four or five books, and one paper. Compile the latex to pdf) (Done)





### Topics for me
- Read the Mathematics of Graphs
- Try out elm and get to know how it works.
- Draw a wire frame on paper. Scan it.
- Write an essay on Isomorphism and Hamiltonian Paths.
- Write Methodology.
- Come up with new easy topics as they would have be studied in a sequential order anyway.  - Talk to the professor if such topics would be okay to add in the dissertation.  - Talk to the professor if topics could be added throughout the period of dissertaiton.  ### Topics for week 4 - Compile latex file and see if the references are going at the right place.
- Read Max k-cut, and tree width. Write up and refrences.
- Drawings of visualization of at least one problem.
- Make notes.

#### Tentative Topics Chosen:
- Graph Isomorphism								(Studied)  
- Hamiltonian Cycle								(Studied)
- Graph Colouring								(Studied)
- Indentification of Independent Set			(Studied)       
- Identification of Cliques in a Graph
- Finding Minimum spanning tree in Graph
- Finding of Vertex cover
- Tree Width
- Max k-Cut

## Week 5
- Researched about different technologies.
- Learned about elm-framework.
- A wireframe was created
- Researched about how to draw simple shapes with svg.
- Researched about how to move svg shapes with time.
- Research was done about tree-width.


## Week 6
- Abstracted over Svg to form vertices and edges.
- Coded to place vertices and edges together to form graphs.
- edges defined as made up of vertices, therefore no need to define
   their positions explicitly.
- Linear algebra, was used to draw polygons on vertices n and define
  edges between the vertices in various fashions. (fully connected, cycles)
- Modified the survey for better.

## Week 7
### Morphing a graph using Grid. 

- Topology (vertices and edges) and name remain the same, display geometry changes. (That is position of the graph)
- Vertices are reconstructed with the new positions
- Edges are created between the new vertices according to the original edges
- All together a new graph is created but it's topology is the same as the original one.
- The new graph can be said to be visually morphed version of the original. But essentially the same graph connection wise and vertex name and
- vertex color wise.

### Put the Edges back.
We are in a way creating a new graph, but it has the vertices of the same name, color but differnt positions
For the edges to be the same as before 
updateEdge takes an (old edge) and a (new list of vertices)
then produces a Edge with new vertices according to the old Edge

### Coloring mechanism
