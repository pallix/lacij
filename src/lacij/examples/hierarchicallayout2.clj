(ns lacij.examples.hierarchicallayout2
  (:use clojure.pprint
        lacij.layouts.layout
        lacij.graph.core
        lacij.graph.svg.graph))

;; TODO: test empty graph
(defn -main
  []
  (let [g (-> (create-graph)
              (add-node :n1 :width 200)
              (add-node :n2a "n2a" :width 200)
              (add-node :n2b "n2b" :width 40)
              (add-node :n3 "n3" :width 200)
              (add-edge (geneid) :n2a :n1)
              (add-edge (geneid) :n2b :n1)
              (add-edge (geneid) :n3 :n2a)
              ;; (layout :hierarchical)
              (build)
              )]
    (pprint g)
    (export g "/tmp/hierarchical2.svg" :indent "yes")))

