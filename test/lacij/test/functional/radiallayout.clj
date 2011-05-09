(ns lacij.test.functional.radiallayout
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
        (lacij.layouts core layout)))

(defn gen-graph
  []
  (-> (create-graph)
      (add-node :r "R")
      (add-node :s "S")
      (add-node :t "T")
      (add-node :u "U")
      (add-node :v "V")
      (add-edge :sr :s :r)
      (add-edge :ts :t :s)
      (add-edge :us :u :s)
      (add-edge :vs :v :s)))

(defn -main []
  (let [g (-> (gen-graph)
              (layout :radial)
              (build))]
    (export g "/tmp/radial.svg" :indent "yes")))
