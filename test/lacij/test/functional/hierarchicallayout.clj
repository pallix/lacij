(ns lacij.test.functional.hierarchicallayout
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
      (add-node :jacket "jacket")
      (add-node :tie "tie")
      (add-node :belt "belt")
      (add-node :pants "pants")
      (add-node :shirt "shirt")
      (add-node :shoes "shoes")
      (add-node :socks "socks")
      (add-node :undershorts "undershorts")
      (add-edge :socks-shoes :socks :shoes)
      (add-edge :under-shoes :undershorts :shoes)
      (add-edge :under-pants :undershorts :pants)
      (add-edge :pants-shoes :pants :shoes "pants-to-shoes" :style {:stroke "darkcyan" :stroke-dasharray "9, :5"})
      (add-edge :pants-belt :pants :belt)
      (add-edge :shirt-belt :shirt :belt)
      (add-edge :shirt-tie :shirt :tie)
      (add-edge :tie-jacket :tie :jacket)
      (add-edge :belt-jacket :belt :jacket)))

(defn gen-graph2
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

(defn gen-graph4
  []
  (-> (create-graph)
      (add-edges [:a1 :p]
                 [:pr :a1]
                 [:po :a1]
                 [:fo :a1]
                 [:a2 :po]
                 [:a3 :po]
                 [:ac :a2]
                 [:pu :a3]
                 [:a4 :ac]
                 [:a5 :ac]
                 [:a6 :ac]
                 [:a7 :ac]
                 [:a8 :pu]
                 [:a9 :pu]
                 [:a10 :pu]
                 [:pur :a4]
                 [:puf :a5]
                 [:pe :a6]
                 [:ab :a6]
                 [:th :a7]
                 [:pos :a7]
                 [:br :a8]
                 [:fl :a9]
                 [:ju :a10]
                 )))

(defn -main
  []
  (let [g (-> (gen-graph2)
              (layout :hierarchical)
              (build))]
    (export g "/tmp/hierarchical.svg" :indent "yes")))

