module Main exposing (..)

import Html as H exposing (div, h1, p, text)
import Html.Attributes as HA exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)


main = view "hello there"

view model =
   div [] [ H.div pageStyle [graphOne, explanationOne]
          , H.div pageStyle [explanationTwo, graphTwo]
          , H.div pageStyle [graphThree, explanationThree]
          , H.div pageStyle [explanationFour, graphFour]
          ]

type alias Pos = {x : Int, y : Int}
type alias Size = Int

theSvg =
    S.svg
     [ SA.width "400"
     , SA.height "1200"
     , SA.viewBox "0 0 400 1200"
     ]
     [circle 20 {x = 200, y = 500}]


graphOne = H.div leftSideStyle [ theSvg ]
explanationOne = H.div rightSideStyle [ H.h1 [] [H.text "Graph Isomorphism"]
                                    , p [] [ H.text isomorphismExplanation]
                                    ]

graphTwo = H.div rightSideStyle [H.text "Graph"]
explanationTwo = H.div leftSideStyle [ H.h1 [] [H.text "Hamiltonian Cycle"]
                                    , H.p [] [ H.text hamiltonianExplanation]
                                    ]

graphThree = H.div leftSideStyle [H.text "Graph"]
explanationThree = H.div rightSideStyle [ H.h1 [] [H.text "Clique"]
                                    , H.p [] [ H.text cliqueExplanation]
                                    ]

graphFour = H.div leftSideStyle [H.text "Graph"]
explanationFour = H.div rightSideStyle [ H.h1 [] [H.text "Clique"]
                                    , H.p [] [ H.text cliqueExplanation]
                                    ]

circle : Size -> Pos -> S.Svg msg
circle size pos =
    S.circle
        [ SA.cx (String.fromInt pos.x)
        , SA.cy (String.fromInt pos.y)
        , SA.r (String.fromInt size)
        , SA.style "fill: red;"
        ]
        []


pageStyle =
  [ HA.style "height" "100vh"
  ]
leftSideStyle =
  [ HA.style "float" "left"
  --, HA.style "background" "Red"
  , HA.style "width" "45%"
  , HA.style "height" "100%"
  , HA.style "padding" "30px"
  , HA.style "margin" "10px"
  ]
  
rightSideStyle =
  [ HA.style "float" "right"
  --, HA.style "background" "Blue"
  , HA.style "width" "45%"
  , HA.style "height" "100%"
  , HA.style "padding" "30px"
  , HA.style "margin" "10px"
  ]

isomorphismExplanation =
   """
   Two graphs G1 and G2 are isomorphic if there is a one-one correspondence
   between the vertices of G1 and G2 such that the number of edges between any
   two vertices in G1 is equal to the number of edges joining the corresponding
   vertices of G2. Here the graphs may appear to be different in appearance and
   the labeling of the nodes and edges. But the way one vertex is connected to
   another in one graph is same to another. Therefore given two graphs,
   detecting if the graphs are Isomorphic is a problem to solve.  One way to
   explain this would be to manipulate the position of vertices and edges to be
   appear same as it's isomorphic counterpart.  We want to show what
   isomorphism is.
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
