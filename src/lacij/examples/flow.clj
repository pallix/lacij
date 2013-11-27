(ns lacij.examples.flow
  (:use clojure.pprint
        lacij.layouts.layout
        lacij.edit.graph
        lacij.view.graphview))

(defn gen-graph
  []
  (-> (graph)
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
  (-> (graph)
      (add-node :jacket "jacket")
      (add-node :tie "tie")
      (add-node :belt "belt")
      (add-node :pants "pants")
      (add-node :shirt "shirt")
      (add-node :shoes "shoes")
      (add-node :socks "socks")
      (add-node :undershorts "undershorts")
      (add-edge :socks-shoes :shoes :socks)
      (add-edge :under-shoes :shoes :undershorts)
      (add-edge :under-pants :pants :undershorts)
      (add-edge :pants-shoes :shoes :pants)
      (add-edge :pants-belt :belt :pants)
      (add-edge :shirt-belt :belt :shirt)
      (add-edge :shirt-tie :tie :shirt)
      (add-edge :tie-jacket :jacket :tie)
      (add-edge :belt-jacket :jacket :belt)))

(defn -main
  []
  (let [g (-> (gen-graph2)
              (layout :hierarchical :flow :out)
              (build))]
    (export g "/tmp/flow2.svg" :indent "yes")))
