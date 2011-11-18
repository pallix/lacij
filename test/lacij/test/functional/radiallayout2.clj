(ns lacij.test.functional.radiallayout2
  (:use clojure.pprint
        lacij.layouts.layout
        lacij.graph.core
        lacij.graph.svg.graph))

(defn add-nodes [g & nodes]
  (reduce (fn [g node]
            (add-node g node (name node)))
          g
          nodes))

(defn add-edges [g & edges]
  (let [g (apply add-nodes g (set (flatten edges)))]
    (reduce (fn [g [src dst]]
              (let [id (keyword (str (name src) "-" (name dst)))]
                (add-edge g id src dst)))
            g
            edges)))

(defn gen-graph
  []
  (-> (create-graph)
      (add-edges [:a :b] [:a :d3]
                 [:b :c] [:b :e4]
                 [:c :d1] [:c :d2]
                 [:d1 :e1] [:d1 :e2]
                 [:d2 :e3] [:d2 :f5]
                 [:d3 :f4] [:d3 :e4]
                 [:e1 :f1] [:e1 :f2]
                 [:e2 :f2]
                 [:e3 :f3] [:e3 :f4]
                 [:e4 :f5] [:e4 :f7]
                 [:e5 :f5] [:e5 :f7] [:e5 :f8]
                 [:f1 :g1]
                 [:f2 :g1]
                 [:f3 :g1] [:f3 :g2]
                 [:f4 :g1]
                 [:f5 :g1] [:f5 :g2]
                 [:f6 :g1] [:f6 :g2]
                 [:f7 :g2]
                 [:f8 :g2]
                 [:f9 :g2])))

(defn gen-graph2
  []
  (-> (create-graph)
      (add-edges [:s1 :r1]
                 [:s1 :r2]
                 [:t1 :s1])))

(defn -main
  []
  (let [g (-> (gen-graph2)
              (layout :radial)
              (build))]
    (export g "/tmp/radial2.svg" :indent "yes")))

