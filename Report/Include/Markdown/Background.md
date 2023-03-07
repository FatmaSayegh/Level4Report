This chapter discusses concepts which are essential to understand the
forthcoming chapters in this report. It starts out by litrature review
discussing the books and research papers refered and consulted for this
project.Then it goes onto discuss Graph Theory definitions and it's
classical problems, which are useful to understand their implementation
in the app. Discussion of prior work on visualization of computer
science topics which have an influence over this project is also an part
of research.

Literature Review
=================

The books and the research papers which were referred to for this
project range from theoretical texts on Graph Theory, to Programming
books on functional programming and the functional programming language
for web front end called Elm.

Graph Theory and Algorithms Literature
--------------------------------------

Parts of the following literature was used to get familiar with the
basics of graph theory, some basic definitions and theorems and also
explanation of some classical graph theory problems. This literature
also played a role in shortlisting the graph theory problems which were
chosen for the purpose of this project.

### Networks: An Introduction.

This book was consulted for definitions, explanations and understanding
of basics of Graph Theory and it's problems. It discusses the
mathematical and computer science perspectives of the subject in
dedicated sections. See [@Newman10].

### Algorithm Design.

This book lent the description and the example of *Tree-width* problem
incorporated for an animation in the application. See for the
explanation of the topic. In general, this book covers most kinds of
data structures and and has excellent portion on mathematical and
computer science aspect of Graph theory. See [@KleinbergTardos06].

Functional Programming and Elm Programming Literature
-----------------------------------------------------

In this section, literature covering functional programming and in
particular Elm programming language which were helpful in this project
is reviewed.

### Why Functional Programming Matters.

This paper is a terse tutorial using a Haskell like programming language
called Miranda, expositing the power of functional and modular thinking
to glue functions together to create and manipulate data structures like
lists and trees. It also discusses recurring patterns in programming and
their abstraction in form of higher order functions such as map, filter,
and the folds. In a steep learning curve it goes onto discuss some
advanced concepts in Artificial Intelligence. See [@Hughes89].

### Elm in Action.

This book can be used as a step by step tutorial introduction to the Elm
language and it's application in creating real world web apps. It starts
with small applications and gradually moves to the art of managing
projects of considerable size. It acts as a bridge between functional
programming and front-end design; it also dwells into front-end design
recipies like how to implement a single page application. See
[@feldman2020elm].

Aesthetics
----------

### The Beauty of Simplicity.

This paper provides arguments for how optimal simplicity, minimalism and
intuitiveness makes a user interface more usable and trustable. This
paper was the inspiration for keeping the website simple with the
minimum of complicated control and features. See [@Karvonen2000].

Discussion of Classical Graph Theory Problems
=============================================

This section formally discusses the concepts of Graph theory which are
elucidated visually in the application. If you are already familiar of
the topics discussed in this chapter then by all means skip over. If
not, then it's recommended to go through the section as the material
discussed here is essential to understand the discussion in the further
chapters. The forthcoming chapters will refer to the subsections here
for definitions and explanations.

Definitions {#graphtheory: definitions}
-----------

A *Graph* $\boldsymbol{G}$, can be understood as a collection of
vertices which are connected to each other by edges. A *Vertex*
$\boldsymbol{v}$ can be understood as a point and an *Edge*
$\boldsymbol{e}$ is a pair of vertices. The *set of all the vertices* in
a graph $G$ is represented as $\boldsymbol{V(G)}$ and the *set of all
the edges* in $G$ is represented as $\boldsymbol{E(G)}$.

For a vertex $v$, it's *degree* $\boldsymbol{deg(v)}$ is the number of
edges connected to it. An *isolated vertex* $v$ is such that
$deg(v) = 0$. An *end vertex* is a vertex $w$ such that $deg(w) = 1$.
Two vertices are *adjacent* to each other if there is an edge connects
them.

A *bipartite* graph, is a graph $G$ such that it's vertices $V(G)$ can
be split into two disjoint sets $A$ and $B$ such that each edge of $G$
joins a vertex of $A$ and a vertex of $B$. See [@Newman10].

Graph Isomporphism
------------------

Two graphs $G_1$ and $G_2$ are isomorphic if there is a one to one
correspondence between the vertices of $G_1$ and $G_2$ such that the
number of edges between any two vertices in $G_1$ is equal to the number
of edges joining the corresponding vertices of $G_2$. Given two graphs,
detecting if the graphs are Isomorphic is a problem to solve as the
graphs may appear to be different in appearance and in the labeling of
the nodes and edges. See [@Newman10].

### Application

The graph isomorphism problem finds application in the field of
bioinformatics for finding network motifs (sub-graphs isomorphic to an
input pattern) in a larger biological network. A network motif is a
recurring pattern of connection of vertices in a large graph signifying
their evolutionary selection over random patterns. See [@Bonnici2013].

Max K Cut
---------

A maximum cut, is partioning the vertices of a graph in two groups such
that the number of edges between these two groups is maximum. In a
weighted graph, where the edges are weighted, the weights of the edges
are also taken into consideration. A maximum k-cut, is generalized
version of maximum cut, where the graph is partitioned into k subsets,
such that the number of edges between these groups is maximized.

It is important to note that a bipartite graph (refer to the Definitions
section above) is a trivial example of Max Cut there are no edges among
the vertices of a set $A$ and no edges among the vertices of set B and
all the edges are from the vertices in set $A$ to vertices in set $B$.

Graph Colouring
--------------

It is an optimization problem where the objective is to assign to the
vertices of a graph a colour such that no two adjacent vertices have the
same colour, while keeping the number of colours employed to a minimum.
Here a colour can be thought of just any symbol from a finite set of
symbols.

Minimum Vertex Cover
--------------------

Minimum Vertex cover of a graph is the minimum amount of vertices such
that, all the edges in the graph must have one of such vertices as at
least one of their endpoints. This is also a optimization problem in
which the constraint is that all the edges must be covered while keeping
the number of vertices in the set of Minimum Vertex Cover to the
minimum.

Tree Width {#explanation: treewidth}
----------

We will explain in two parts. First we will define what a tree
decomposition of a graph is. Then we will define tree width of the
graph.

To decompose a Graph in a tree is to put nodes into sets called pieces,
subject to certain conditions. The first condition is that all the
vertices of $G$ should belong to at least one piece. Every edge of $G$,
must be present in at least one piece which contains both ends of the
edge. And finally, in the tree decomposition, if there is a node $n$
present in a walk from a node $n1$ to $n2$, and if both $n1$ and $n2$
have a vertex $v$ in common, then the node $n$ also contains that vertex
$v$.

Any graph can be decomposed into a tree. Trivially, a graph can be tree
decomposed by putting all of it's vertices in just one node. But it will
not be a very useful tree decomposition. Therefore a good tree
decomposition of a tree is the one which has small pieces. Tree width is
defined as the size of the biggest piece $V_t - 1$. The smaller the tree
width the bigger the better the tree decomposition. See
[@KleinbergTardos06] in the bibliography.

Prior Work
==========

This project takes subtle inspirations from some of the work which is
available on the internet as web applications for visualization of
popular algorithms. Although the works which are discussed in this
section are focused on understanding algorithmic solutions of computer
science problems, the visualization of graphs, trees and lists in such
projects have been inspiring for depiction of graphs and their animation
in this project.

Data Structure Visualizations {#priorWork: datastrucvisu}
-----------------------------

This tool was developed by David Galles, Associate Professor, University
of San Francisco. See [@Galles], in the bibliography section. It covers
topics from various categories of computer science problems such as
Dynamic Programming, Geometric Programming, Trees, Heaps, Graphs etc.

The design of the tool has several important features. The task of
forming the data-structures as input to algorithms is quite often
delegated to the user rather than they being predefined or hard-coded.
There are control buttons which allow the user to start pause and
restart the animations. There is a slider to tune the speed of the
animation as well.

There is a dearth of textual explanation of the algorithms while they
run. Perhaps, the main purpose of this tool is as a teaching aid such
that the teacher first explains the topic and uses the tool as a visual
demonstration to show his students the working of the algorithm on real
datastructures.

VisuAlgo {#priorWork: visualgo}
--------

This tool was developed by Dr. Steven Halim of National University of
Singapore. See [@HalimVisu] in the bibliography. It covers topics from
the subject of data structures and algorithms. Most relevant for this
project are the topics related to graph theory; which are Maximum Flow,
Minimum Vertex Cover, Traveling Salesman and Steiner Tree, although the
emphasis is on algorithmic solutions to the problems and not problem
visualization which is the emphasis of this project.

For most topics the user is able to construct his instances of
datastructures. Unlike Data Structure Visualization application
mentioned in , there is an ample amount of textual information in terms
of theory, tutorial and instructions. The explanation of the topics is
done in text blocks which appear at appropriate places in a slide show
like fashion. For organizing the textual information, it has drop down
content menu for easy access to various sections.

Algmatch {#priorWork: algmatch}
--------

This Web app was developed as a final year individual project
dissertation for by Liam Lau under the supervision of Sofiat
Olaosebikan. The application visualizes the matching algorithms such as
Gale-Shapley Stable Matching and Extended Gale-Shapley Stable Matching
algorithms applied to stable marriage and hospital/residents problem.
See [@LiamApp] in the bibliography. The app lends ideas about user
friendliness and intuitive usage.

It has a panel which describes the algorithm steps while in an animation
the matching algorithm works on an instance of the problem.

Aesthetically, the most noteworthy features of the app are, playback and
speed controls which are as intuitive as media buttons on a media player
and smooth page animations resulting in a pleasing user interaction.
