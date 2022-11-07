Appropriate divs for display of SVGs (animation) and text were determined.

What :
- Data type Vertex has a name (String) and a 2D position. Data Type Edge contains two vertices.
- Data type Graph has a list of Vertices and a list of Edges.
- Abstracting out the details of using native functions of svg (circle and line) by functions for drawing a Vertex and an Edge.
- Abstracting out the details of Vertex and Edge by a function to draw Graph.

- Currently creation of a vertex requires a 2D position on the designated co-ordinate system.
   - As of now the only way to place a vertex on the co-ordinate system is rectiliear x and y.
   - With incorporation of Homogenous matrices (Computer graphics), we can abstract complicated placement of vertices
     so that in the future we have.
   - Abstracting out the lower details of drawing (determing position) by algebra.
