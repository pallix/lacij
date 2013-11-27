(ns lacij.examples.flow-radial
  (:use lacij.edit.graph
        lacij.view.graphview
        (lacij.layouts core layout)))

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

(defn gen-graph3
  []
  (-> (graph :width 800 :height 600)
      (add-default-node-attrs :width 25 :height 25 :shape :circle)
      (add-nodes :r :s :t :u :v :w :x :y :t1 :t2 :t3 :t4 :t5
                 :v1 :v2 :v3 :u1 :u2 :w1 :w2
                 :x1 :x2 :x3 :x4 :x5 :y1 :y2 :y3)
      (add-edges [:r :s] [:s :t] [:s :u] [:s :v]
                 [:t :t1] [:t :t2] [:t :t3] [:t :t4] [:t :t5]
                 [:u :u1] [:u :u2] [:v :v1] [:v :v2] [:v :v3]
                 [:r :w] [:w :w1] [:w :w2] [:w :y]
                 [:y :y3] [:y :y2] [:y :y1]
                 [:r :x] [:x :x1] [:x :x2] [:x :x3] [:x :x4] [:x :x5])))

(defn -main []
  (let [g (-> (gen-graph3)
              (layout :radial :radius 90 :flow :out)
              (build))]
    (export g "/tmp/flow-radial.svg" :indent "yes")))
