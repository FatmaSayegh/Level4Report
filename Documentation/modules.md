# Modules

The program is divided into ten modules.  $Main$ holds a key role and acts as the launching pad for the program. It also serves as a connector, loading the features of specific graph theory topics as needed when navigation commands are issued.

The five topics included in the project own a module of their own. They are
`Isomorphism`,`Maxkcut`, `Graphcoloring`, `VertexCover` and `TreeWidth` modules.
These modules are imported by the main function to be used on the screen.
Henceforth these will be collectively known as the Modules.

Within the $Graph$ module there are data types and functions for representing,
constructing, drawing and animating the graphs. This module contains most of the
code related to geometry. This module is depended on by all the topic modules.

The `Explanation` module contains the text data as a `0-nary` functions
(functions which do not take input) used by topic modules for the explanation
of the respective topics. These only contain the definitions of the concerned
graph theory problems.

Finally, the $Messages$ module contains the messages which are generated with
system clock ticks or user interaction with the DOM elements.  Such messages
are defined as Algebraical Data Types} and are imported by almost all other
modules. This along with the $Main$ and $Graph$ modules form the central pillar
of the program and are the minimum requirement of the program to function.
