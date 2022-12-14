This document discusses the design choices made for the application and
the guiding principles behind such choices. It progresses to how a graph
are visually displayed and animated in the app without going in the
technical details of implementation. Finally, I shall discuss the
intended experience of the user while going through the various topics
explained in the application. We will also discuss the learning impact
of each topic on the user as it forms an essential aspect of overall
design.

Guiding Principles
==================

Simplicity {#design: simplicity}
----------

For the purpose of elucidation of Mathematical concepts, which requires
an undivided attention of the learner, it was decided that the user
interface must have the minimum amount of clutter possible, without
giving away the minimum amount of functionality required. The learner
should not get distracted by an overpopulated user interface. Simplicity
and elegance would also lead the user to stay longer on the application
without getting visually exhausted. Furthermore the users on the web
today are extremely goal driven and don't want any obstruction between
them and their goal. See [@Karvonen2000].

Intuitiveness
-------------

The layout of the user interface should be such that it not only has
utility, but should also help communicate the intention of the designer
about the usage of the application. For example a play button, just like
it was found on media devices for decades, invites the user to
kick-start an animation even without going through the text which tells
him to do so explicitly. The size and placement of the play button on
the page, therefore becomes important. Right placement of user
interactive elements guide the user through the story which is intended
to be told.

Meaningfulness
--------------

For a substantial learning impact, the elucidation of the topics must
reach at the heart of the topic. Furthermore they must have a story line
which is meaningful and coherent. The learning outcomes of the animation
or a user-interactive task must be well defined before an attempt is
made to implement them.

Wire-frame and Navigation
=========================

The web application is a **Single Page Application**. Where the
navigation from one topic to another occurs according to the user
inputs. When the state moves from one graph theory to another topic, the
data on the screen, that is the graphics and the text, change on the
same page without loading a new HTML each time. The user however will
notice the url change with navigation from one topic to another. This
will also give the user the ability to use forward and backward buttons
in the browser to navigate through the history of URLs visited. As a new
HTML page is not loaded every time the user navigates from one topic to
another the screen therefore does not turn briefly white and the
transitions are imperceptible by default.

There were several iterations made for the layout of the web page, one
of the is shown in . Finally, a layout was chosen, see , in which the
page is vertical divided into two parts, the left part of the page
contains an instance of an animation, the right contains explanation of
the topic and advice on how to interpret the animation along with
navigation and control buttons.

The text in the explanation part of the page is dynamic in nature, if
the animation has facility of user interaction with it's elements, the
corresponding text on the right responds with advises on the state of
affairs and what the user should do next.

There is a navigation bar at the bottom of the page, with left and right
arrows, along with the names of the previous and the next topics, to hop
from one topic to another.

![The final wireframe](WireframeInitial){#fig:pubsub2
width=".9\\linewidth"}

![The final wireframe](Wireframe){#fig:pubsub2 width=".9\\linewidth"}

Animation Panel
===============

As mentioned in the preceding section, the left half of the page is for
graphics, which contains an animation, a user-interaction session or a
combination of both. The animation contains one or more than one graphs.
These graphs undergo, according to needs of the topic in consideration
transformations of appearance and annotation.

Visual representation of graphs
-------------------------------

The graphs are represented as vertices and edges joining those vertices.
Although the geometrical placement of the vertices is of no consequence
in the subject of graph theory, for the purpose of visualization,
vertices are assigned a 2 dimensional position. The edges, don't contain
attributes such as length or positions of endpoints of a line segment,
they are rather defined as a relation between a pair of vertices.

### Appearance of Vertices

The vertices of graphs in the animations are color filled circles of the
same size save for certain exceptions. The color of the vertices have
been allotted by varying mostly the hue and keeping saturation and
lightness relatively same in the **HSL** (Hue Saturation Lightness)
color space. The vertices contain a name inside the circle as an
integer. The names of vertices were chosen as integers as it was assumed
that such a representation would help the developer and also the user to
keep a track of them as order of integers is understood better by
humans.

When a particular vertex is needed to be shown differently than the rest
then it's size and color are displayed differently. For example, a
user-selected vertex in some of the animations are shown bigger and it's
color changed to golden. The gold color it was observed makes the vertex
in consideration stand out differently from other colors in the
animation.

### Appearance of Edges

Edges, although are defined very algebraically as a relationship between
a pair of vertices it gets drawn by referring to the positions of the
related vertices as a straight line segment white in color.

When an edge is supposed to shown differently than the rest of the
edges, it's width is increased and color changed to the same value of
golden as a selected vertex. Again it was found that this color and
thickness, made the edge standout from the rest of the edges and helped
in showing it distinguished without being unpleasantly distracting.

Explanation Panel
=================

Whereas, the animation panel on the left page contains all the graphical
components of the topic elucidation. The right half of the page is
occupied by the Explanation Panel. It contains the title of the problem
being explained, it's definition and instruction on how to go ahead with
starting the animation or user-interactive tasks. It also contains
control buttons for starting, re-starting and pausing animations. For
user-interactive tasks it has buttons to reset the task.

As the animation or a user-interactive task progresses the explanation
panel generates explanatory and instructive text.

User Stories for Elucidation of Topics
======================================

This section describes, what the user is intended to experience while
interacting with the individual topics in the application with a special
consideration towards learning impact and understandibility.

When the user opens the web application on a browser of her choice, the
first topic she sees is that of Graph Isomorphism. She may stay there to
interact with the example of the topic or navigate to other topics using
navigation buttons at the bottom to have a bird's eye view of other
topics.

Each graph theory problem has it's own character and require a different
approach for elucidation. The following sections will explain these
approaches with their intended experience on the user with learning
outcomes which may be achieved.

The survey mentioned in the , also gave quite a few suggestions on
different ways to elucidate the short listed topics. The suggestions
were very topic specific, and included various ideas such as animations
and games and the ability to construct user defined graphs etc. Some of
the suggestions could be included in the project, some could not be
accomodated as they did not fit the flow of narrative, while others
while being brilliant ideas, act as inspiration for future work due to
constraints of time.

Graph Isomorphism {#story: isomorphism}
-----------------

The user is presented with a graph on the left and textual explanation
on the right of the page. The textual explanation portion of the page
also has some media buttons such as play/pause and reset to interact
with the explanation. The text explanation briefly defines graph
isomorphism and advises the user to press the play button.

### Animation:

When the user presses the play button, a new graph emerges out of the
old one while the keeping the edges between any two vertices conserved.
While keeping the connectivity between the vertices intact, the graph
transforms into a completely new shape, almost giving a visual proof
that the two graphs on the screen are isomorphic to each other.

### User Interaction:

After the two isomorphic graphs have separated from each other, the user
is advised in a text panel to choose a vertex by either hovering over a
vertex or pressing the corresponding number on the keyboard. Doing so,
will change the visual appearance of the selected vertex, the edges
incident on the selected vertex and the adjacent vertices to the
selected vertex in both graphs. The selected vertex will be enlarged to
a new radius and change its color from it's original color to golden
color making it stand apart from the rest of the vertices. The edges
incident on the vertex will change their colors to the same gold color.
The adjacent vertices to the selected vertex will form a golden halo
around them. This color transformation will distinguish a kind of a
subset in the two displayed graphs. At this point of time, in the text
panel the user is pointed out that the selected vertex has the same
number of edges connecting to the same adjacent vertices in both the
graphs. The user is also advised to inspect other vertices of the graphs
and convince herself that each vertex of the graph has the same adjacent
vertices in both the isomorphic counterparts.

### Learning Impact:

The transformation of a graph into a radically different looking graph
but being essentially the same as far as the connectivity between the
vertices go acts as a visual proof that the graphs are isomorphic. While
individually inspecting each vertex will re-confirm this idea to the
user. After having experienced the concept of graph isomorphism in this
way it is assumed that the concept and definition of the term would be
clear to her.

Max k Cut {#story: maxkcut}
---------

Max Cut has two animations one after the other. The first animation is
about Max 2 Cut and the second is about Max 3 Cut. It is assumed that
the user will extrapolate the concept of the general Max k Cut after
understanding the first two cases ($k=2$, $k=3$) and extrapolating it
over greater values of k. The examples shown in the animations are a
nearly bipartite and a tripartite graph for $k=2$ and $k=3$
respectively. A nearly bipartite and a tripartite graph is used to
elucidate the topic as it is easier for the user to visualize how the
two graphs can be segregated to sets of vertices such that the maximum
number of edges pass between such sets.

In both the cases of $k=2$ and $k=3$, the weight of all the edges is
taken to be equal to $1$. This decision has been taken as with $w=1$,
the answers to both the max cut problems are more visual than unequal
weights.

### Max 2 Cut Animation: {#story: max2cut}

It starts with an Original graph on the left and definition of Max K Cut
and Max 2 Cut on the right of the web page. The right part of the page
also contains media buttons to pause and play the animations. It also
has a button for switching from Max 2 Cut example, to Max 3 Cut example.
The Max 2 Cut animation starts with a graph, which starts when the user
presses the play button. As the animation progresses a new graph emerges
out of the original one and translates towards right changing it's shape
to segregate it's vertices into two sets forming a Maximum 2 Cut. The
two sets move vertically up and down and increase the distance between
themselves, revealing the number of edges passing from one set to
another which the user can intuitively tell is greater than the number
of edges between any other two sets which may have been formed from the
vertices of the graph.

The user is advised to put up a pre-defined horizontal line by pressing
a button in the explanation panel. The line is drawn between the two
sets of vertices. The intersection points between the edges and the max
cut line is shown by blue dots. These user is advised to observe the
number of intersection points which tell the user the number of edges
passing from one set to another.

![Initial design of Max 2 Cut](maxcutdes)

### Max 3 Cut Animation: {#story: max3cut}

Just like the animation for Max 2 Cut, this animation is started by the
user by pressing the play button. The animation on the right starts with
a tripartite graph arranged in a circular form. As the animation
progresses the graph gets Divided into three sets of vertices in which
the three sets translate in directions which are set $120^{\circ}$ apart
from each other. The graph transforms from a circularly arranged one to
a triangular form. The user can draw the Max 3 Cut lines at any point in
the progress of the animation. These are three lines separating each set
from the rest of the graph, with points of intersection shown in blue
showing the number of edges passing from one set to the rest of the
sets.

### Learning Impact:

Although the examples in the Max 2 Cut and Max 3 Cut can be seen as
simple ones as the first one was nearly a bipartite graph and the second
one was a tripartite graph, they do a good job at defining the problem
well. Not just that such visualization explains the problem well it also
may act as artwork especially in the case of Max 3 Cut, which may
inspire an imaginative student to want to investigate the subject
further.

Graph Coloring {#story: coloring}
--------------

### User Interaction:

A user-interactive task was chosen for explaining Graph Coloring as the
nature of the problem lends naturally for such. The user is presented
with a graph which has all the vertices in color white. On the
explanation panel to the right he is given the definition of the problem
and advice on how to complete the task.

The task is to choose colors from a color palette of three colors namely
red, green and blue, and color vertices in the graph such that no two
adjacent vertices have the same color. Whenever the user colors two or
more adjacent vertices the same color the text panel warns them to make
amends. There is a reset button in the explanation panel to un-color all
the vertices to start all over again if the user wants to start from the
beginning.

The user is challenged to first challenged to color the graph
successfully in just two colors but as it would be soon clear to the
user it can only be done in three.

A user-interactive task was chosen for explaining Graph Coloring as the
nature of the problem lends naturally for such.

### Learning Impact:

The user interactive task will help the student retain the meaning of
the Graph Coloring problem in their memory for a long period of time
than just watching an animation as a spectator.

Minimum Vertex Cover {#story: vertexcover}
--------------------

### User Interaction:

Minimum Vertex Cover, by the nature of the problem is chosen to be
elucidated by the help of a user-interactive task. The user is given a
graph along with explanation of he Minimum Vertex Cover problem. He is
also explained how to complete the task. The task is to select vertices
successively either by clicking them or pressing a number corresponding
to the vertex of choice on the keyboard. When he selects the vertex, the
selected vertex and all the edges incident on it are displayed
differently. He has to thus highlight all the edges by selecting the
minimum number of vertices in the graph. If he has done this task
effectively then he would not choose more vertices than required to
cover all the edges in the graph. When the user covers all the edges by
only selecting four vertices, he is given a congratulatory message for
having done the task right. If he covers the graph by selecting more
than four vertices then he is advised to do the same in just four.

### Learning Impact:

Just like Graph coloring, in the case of Minimum Vertex Cover too, it is
assumed that a user-interactive task is effective not just in
explanation of a topic but also, retention of the concept for a long
period of time.

Tree Width
----------

### Animation:

The topic Tree Width is explained in a multi-part animation. The first
part begins with a graph in which the vertices are arranged in a
circular pattern. The circular form conceals a tree like structure which
can be abstracted out of the graph.

The user while reading the explanation is instructed to press the
'forward' button to move to the first part. The user hops from one part
of the animation to another by pressing this 'forward' button. In the
first part the graph which was hitherto arranged in a circular pattern
transforms into a regular lattice like pattern. The tree-like pattern is
more apparent in the new visual form of the graph. This is also pointed
out in the explanation panel.

The next part of the animation shows an example of a piece (a sub-graph)
containing three vertices against the backdrop of the graph. The
significance of pieces in the tree-width concept is discussed in the
background chapter. The piece is also represented by a blue dot at the
centroid of the three vertices. In the next part of the animation the
whole of the graph is marked by it's constituent pieces by blue dots.

In the final part, the pieces form the nodes of a tree. The tree's edges
(branches) are colored in golden color to make it stand out from the
graph in the background. At this point, the definition of the tree width
is given in the explanation panel.

### Learning Impact:

The user is expected to learn the concept of tree decomposition of a
graph. Also, by the help of animations, he will be inspired to learn
abstract thinking: how a *form* of a tree can be derived out of an
unassuming typical graph.
