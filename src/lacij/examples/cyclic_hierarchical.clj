(ns lacij.examples.cyclic-hierarchical
  (:require [lacij.edit.graph :refer :all]
            [lacij.model.graph :refer :all]
            [lacij.view.graphview :refer [export]]
            [lacij.layouts.layout :refer :all]
            [lacij.layouts.utils.cycle :refer [break-cycles]]
            [lacij.utils.core :refer :all]))

(defn gen-graph
  []
  (-> (graph :width 800 :height 600)
      (add-default-node-attrs :width 25 :height 25 :shape :circle)
      (add-node :n1 "n1")
      (add-node :n2 "n2")
      (add-node :n3 "n3")
      (add-node :n4 "n4")
      (add-edge :e12 :n1 :n2)
      (add-edge :e23 :n2 :n3)
      (add-edge :e31 :n3 :n1)
      (add-edge :e24 :n2 :n4)
      ))

(defn -main
  []
  (let [g (-> (gen-graph)
              (layout :hierarchical)
              (build)
              )]

    (export g "/tmp/cyclic-hierarchical.svg" :indent "yes")
    ))
