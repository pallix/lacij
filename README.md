# Lacij

Lacij is a graph visualization library written in Clojure. It allows the **display
and the dynamic modification of graphs as SVG documents** that can be viewed
with a **Web browser** or with a **Swing component**. Undo/redo is supported for the
dynamic modification. Automatic layout is provided for the visualization.

## Presentation

An online presentation of Lacij, dated from June 2011, can be found here:
https://docs.google.com/present/view?id=dsjwfrk_1js9ptkcd

## Installing

The easiest way to use Lacij in your own projects is via
[Leiningen](http://github.com/technomancy/leiningen). Add the following
dependency to your project.clj file:

    [lacij "0.8.0"]
    
    
## Usage

### Example 1: creating a simple graph and exporting it as SVG.

    (let [g (-> (graph :width 400 :height 400)
                  (add-node :hermes "Hermes" :x 10 :y 30)
                  (add-node :zeus "Zeus" :x 200 :y 125)
                  (add-node :ares "Ares" :x 200 :y 225)
                  (add-edge :father1 :hermes :zeus)
                  (add-edge :father2 :ares :zeus)
                  (build))]
        (export g "/tmp/simple.svg" :indent "yes"))

![Simple graph](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/simple.png)

### Example 2: using SVG styles to customize nodes and edges

    (-> (graph :width 800 :height 400)
        (add-default-node-style :fill "lightgreen")
        (add-default-edge-style :stroke "royalblue")
        (add-node :hermes "Hermes" :x 10 :y 30 :style {:fill "lightblue"})
        (add-node :zeus "Zeus" :x 300 :y 150 :rx 15 :ry 15)
        (add-node :ares "Ares" :x 300 :y 250 :style {:fill "lavender" :stroke "red"})
        (add-edge :father1 :hermes :zeus "son of"
                    :style {:stroke "darkcyan" :stroke-dasharray "9, 5"})
        (add-edge :father2 :ares :zeus)
        (add-default-node-attrs :rx 5 :ry 5)
        (add-node :epaphus "Epaphus" :x 450 :y 250)
        (add-edge :epaphus-zeus :epaphus :zeus)
        (add-node :perseus "Perseus" :x 600 :y 150)
        (add-edge :perseus-zeus :perseus :zeus)
        (add-label :father2 "son of" :style {:stroke "crimson"
                                             :font-size "20px"
                                             :font-style "italic"})
        (build)
        (export "/tmp/styles.svg"))

![Graph with styles](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/styles.png)


### Example 3: automatic layout

Radial Layout:

![Radial layout](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/radial.png)

Hierarchical Layout:
![Hierarchical layout](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/hierarchical.png)

Naive Layout (a simple layout used to test the library):
![Naive layout](https://github.com/pallix/lacij/raw/master/resources/lacij/examples/layout1.png)

## Learning by doing

__All examples are available in the__ [examples directory](https://github.com/pallix/lacij/tree/master/src/lacij/examples/).

You can run the examples with the following command:

    lein run -m <namespace-of-the-example>

For example:

    lein run -m lacij.examples.undoredo

## Contribution

If you want to contribute and help this project, you can implement a layout algorithm of your choice.

See [http://www.graphviz.org/Theory.php](http://www.graphviz.org/Theory.php) for some references.

## Work in progress

Bug fixes and API improvements.


## License

Copyright (C) 2010-2013 Fraunhofer Gesellschaft

Distributed under the Eclipse Public License, the same as Clojure.

