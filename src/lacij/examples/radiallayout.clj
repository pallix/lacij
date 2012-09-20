(ns lacij.examples.radiallayout
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
        (lacij.layouts core layout)))

(defn gen-graph5 ;; KO
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

(defn gen-graph ;; OK
  []
  (-> (create-graph)
      (add-nodes :r :s :t :u :v :w)
      (add-edges [:s :r] [:t :r] [:u :r] [:v :u] [:w :u])
      ))

(defn gen-graph2 ;; OK
  []
  (-> (create-graph)
      (add-nodes :r :t1 :t2 :t3 :u1 :u2)
      (add-edges [:t1 :r] [:t2 :r] [:t3 :r] [:u1 :t2] [:u2 :t2])
      ))

(defn gen-graph4 ;; OK
  []
  (-> (create-graph)
      (add-nodes :r :t1 :t2 :t3)
      (add-edges [:t1 :r] [:t2 :r] [:t3 :r])
      ))


(defn gen-graph3
  []
  (-> (create-graph :width 800 :height 600)
      (add-default-node-attrs :width 25 :height 25 :shape :circle)
      (add-nodes :r :s :t :u :v :w :x :y :t1 :t2 :t3 :t4 :t5
                 :v1 :v2 :v3 :u1 :u2 :w1 :w2
                 :x1 :x2 :x3 :x4 :x5 :y1 :y2 :y3)
      (add-edges [:s :r] [:t :s] [:u :s] [:v :s]
                 [:t1 :t] [:t2 :t] [:t3 :t] [:t4 :t] [:t5 :t]
                 [:u1 :u] [:u2 :u] [:v1 :v] [:v2 :v] [:v3 :v]
                 [:w :r] [:w1 :w] [:w2 :w] [:y :w]
                 [:y3 :y] [:y2 :y] [:y1 :y]
                 [:x :r] [:x1 :x] [:x2 :x] [:x3 :x] [:x4 :x] [:x5 :x])))

(defn -main []
  (let [g (-> (gen-graph2)
              (layout :radial :radius 90)
              (build))]
    (export g "/tmp/radial.svg" :indent "yes")))
