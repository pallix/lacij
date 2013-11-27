(ns lacij.examples.radialloop
  (:use clojure.pprint
        lacij.layouts.layout
        lacij.model.graph
        lacij.edit.graph
        lacij.view.graphview))

(defn add-nodes [g & nodes]
  (reduce (fn [g node]
            (add-node g node (name node)))
          g
          nodes))

(defn add-edges [g & edges]
  (reduce (fn [g [src dst]]
            (let [id (keyword (str (name src) "-" (name dst)))]
             (add-edge g id src dst)))
          g
          edges))

(defn tight-loop []
  (-> (create-graph :width 800 :height 600)
      (add-default-node-attrs :width 25 :height 25 :shape :circle)
      (add-nodes :a :b)
      (add-edges [:a :b] [:b :a])))

(defn loose-loop []
  (-> (create-graph :width 800 :height 600)
      (add-default-node-attrs :width 25 :height 25 :shape :circle)
      (add-nodes :a :b :c)
      (add-edges [:a :b] [:b :c] [:c :a])))

(defn -main []
  (let [g (-> (loose-loop)
              (layout :radial :radius 90)
              (build))]
      (export g "/tmp/radial.svg" :indent "yes")))
