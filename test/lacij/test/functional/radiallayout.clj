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

(defn gen-graph2
  []
  (-> (create-graph)
      (add-node :r "R")
      (add-node :s "S")
      (add-node :u "U")
      (add-node :v "V")
      (add-node :w "W")
      (add-node :x "X")
      (add-edge :sr :s :r)
      (add-edge :us :u :s)
      (add-edge :vs :v :s)
      (add-edge :wv :w :v)
      (add-edge :xw :x :w)
      (add-edge :xu :x :u)))

(defn -main []
  (let [g (-> (gen-graph2)
              (layout :radial)
              (build))]
    (export g "/tmp/radial.svg" :indent "yes")))
