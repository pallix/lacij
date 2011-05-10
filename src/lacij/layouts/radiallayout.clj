;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns lacij.layouts.radiallayout
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
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

(defn add-to-tree
  [nid graph nextborder visited layer tree layers-data]
  (if (contains? visited nid)
    [nextborder visited tree layers-data]
    (let [layers-data (assoc-in layers-data [:nodes nid] layer)
          layers-data (assoc-node-to-layer layers-data layer nid)
          children (in-children graph nid)]
      [(concat nextborder children) (conj visited nid) tree layers-data])))

(defn build-tree
  [graph border visited layer tree layers-data]
  (if (seq border)
    (let [[nextborder visited tree layers-data]
          (reduce (fn [[nextborder visited tree layers-data] nid]
                    (add-to-tree nid graph nextborder
                                 visited layer tree layers-data))
                  [() visited tree layers-data]
                  border)]
      (prn "nextborder =")
      (pprint nextborder)
      (recur graph nextborder visited (inc layer) tree layers-data)
      )
    [tree layers-data]))

(defn label-sizes-helper
  [graph layers-data])

(defn label-sizes
  [graph layers-data]
  
  )

(defn layer-nodes
  [graph]
  (let [rootnode (find-root graph)
        tree (create-graph)
        [tree layers-data] (build-tree graph
                                       [rootnode]
                                       #{}
                                       0
                                       tree
                                       {:layers [] :nodes {}})
        data (label-sizes graph layers-data)]
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
