# lacij

Lacij is a graph visualization library written in Clojure. It allows the display
and the dynamic modification of graphs as SVG documents that can be viewed
with a Web browser or with a Swing component. Undo/redo is supported for the
dynamic modification. Automatic layout is provided for the visualization.

## Presentation

An online presentation of Lacij, dated from June 2011, can be found here:
https://docs.google.com/present/view?id=dsjwfrk_1js9ptkcd

## Usage

### Example 1: creating a simple graph and exporting it as SVG.

    (let [g (-> (create-graph :width 400 :height 400)
                  (add-node :hermes "Hermes" 10 30)
                  (add-node :zeus "Zeus" 200 125)
                  (add-node :ares "Ares" 200 225)
                  (add-edge :father1 :hermes :zeus)
                  (add-edge :father2 :ares :zeus)
                  (build))]
        (export g "/tmp/simple.svg" :indent "yes"))

![Simple graph](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/simple.png)

### Example 2: using SVG styles to customize nodes and edges

    (-> (create-graph :width 800 :height 400)
        (add-default-node-style :fill "lightgreen")
        (add-default-edge-style :stroke "royalblue")
        (add-node :hermes "Hermes" 10 30 :style {:fill "lightblue"})
        (add-node :zeus "Zeus" 300 150 :rx 15 :ry 15)
        (add-node :ares "Ares" 300 250 :style {:fill "lavender" :stroke "red"})
        (add-edge :father1 :hermes :zeus "son of"
                    :style {:stroke "darkcyan" :stroke-dasharray "9, 5"})
        (add-edge :father2 :ares :zeus)
        (add-default-node-attrs :rx 5 :ry 5)
        (add-node :epaphus "Epaphus" 450 250)
        (add-edge :epaphus-zeus :epaphus :zeus)
        (add-node :perseus "Perseus" 600 150)
        (add-edge :perseus-zeus :perseus :zeus)
        (add-label :father2 "son of" :style {:stroke "crimson"
                                             :font-size "20px"
                                             :font-style "italic"})
        (build)
        (export "/tmp/styles.svg"))

![Graph with styles](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/styles.png)


### Example 3: automatic layout

Radial Layout:

![Automatic layout](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/radial.png)

Naive Layout (a simple layout used to test the library):
![Automatic layout](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/layout1.png)

All the full examples and other examples can be found in the test directory.

## Contribution

If you want to contribute and help this project, you can implement a layout algorithm of your choice.

See [http://www.graphviz.org/Theory.php](http://www.graphviz.org/Theory.php) for some references.

## Work in progress

The algorithm defined in the following paper is currently being implemented:

How to draw a directed graph by Peter Eades, Kozo Sugiyama 
Journal of Information Processing (1990)
Volume: 13, Issue: 4, Publisher: IEEE, Pages: 13-17

## License

Copyright (C) 2010-2011 Fraunhofer Gesellschaft

Distributed under the Eclipse Public License, the same as Clojure.

