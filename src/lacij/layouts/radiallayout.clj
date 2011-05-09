;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns lacij.layouts.radiallayout
  (:use clojure.pprint
        lacij.graph.core
        lacij.layouts.core))

(defn in-children
  [graph nid]
  (let [n (node graph nid)]
   (concat (map #(src (edge graph %)) (in-edges n))
           ;; (map #(dst (edge graph %)) (out-edges n))
           )))

(defn find-root
  [graph]
  (let [allnodes (nodes graph)]
    (first
     (reduce
      (fn [[rootid noutedges] nodeid]
        (let [out (count (out-edges (node graph nodeid)))]
          (if (< out noutedges)
            [nodeid out]
            [rootid noutedges])))
      [(first allnodes) (count (out-edges (node graph (first allnodes))))]
      allnodes))))

(defn assoc-node-to-layer
  [layers-data layer node]
  (let [nodes (get (:layers layers-data) layer)
        nodes (conj nodes node)
        layers (assoc (:layers layers-data) layer nodes)]
    (assoc layers-data :layers layers)))

(defn layer-nodes-helper
  [graph nextnodes layers-data]
  (if-let [[currentnode layer] (first nextnodes)] 
    (let [layers-data (assoc-in layers-data [:nodes currentnode] layer)
          layers-data (assoc-node-to-layer layers-data layer currentnode)
          chil (in-children graph currentnode)
          nextnodes (rest nextnodes)]
      (if (empty? chil)
        (layer-nodes-helper graph nextnodes layers-data)
        (layer-nodes-helper graph
                            (concat nextnodes (map (fn [a] [a (inc layer)]) chil))
                            layers-data)))
    layers-data)
  )

(defn layer-nodes
  [graph]
  (let [rootnode (find-root graph)
        layers-data (layer-nodes-helper graph
                                        [[rootnode 0]]
                                        {:layers [] :nodes {}})]
    (pprint "data =")
    (pprint layers-data)
    
    )
  )

(defrecord RadialLayout
    []
  Layout

  (layout-graph
   [this graph options]
   (let [layers (layer-nodes graph)]
     (pprint layers))
   graph
   ))

(defn radiallayout
  []
  (RadialLayout.))
