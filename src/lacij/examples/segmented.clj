(ns lacij.examples.segmented
  (:require [lacij.edit.graph :refer :all]
            [lacij.model.graph :refer :all]
            [lacij.view.graphview :refer [export]]
            [lacij.layouts.layout :refer :all]
            [lacij.layouts.utils.cycle :refer [break-cycles]]
            [lacij.utils.core :refer :all]))

(defn gen-graph
  []
  (-> (graph)
      (add-node :a "a")
      (add-node :b "b")
      (add-node :c "c")
      (add-node :d "d")
      (add-edge :ba :b :a)
      (add-edge :cb :c :b)
      (add-edge :dc :d :c)
      (add-edge :da :d :a)))

(defn -main
  []
  (let [g (-> (gen-graph)
              (layout :hierarchical)
              (build))]
    (export g "/tmp/segmented.svg" :indent "yes")))
