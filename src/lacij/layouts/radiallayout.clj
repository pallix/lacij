;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns lacij.layouts.radiallayout
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
        lacij.layouts.layout))

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

(def geneid (partial (comp keyword gensym) "e"))

(defn add-to-tree
  [tree nid children added]
  (printf "nid = %s added = %s\n" nid added)
  (let [tree (add-node tree nid (str (name nid)))
        added (conj added nid)]
    (reduce (fn [[tree added] child]
              (if (contains? added child)
                [tree added]
                (let [tree (add-node tree child (str (name child)))
                      eid (keyword (str (name nid) (name child)))]
                  [(add-edge tree eid child nid) (conj added child)])))
            [tree added]
            children)))

(defn explore-border
  [nid graph nextborder visited added layer tree layers-data]
  (if (contains? visited nid)
    [nextborder visited added tree layers-data]
    (let [layers-data (assoc-in layers-data [:nodes nid] layer)
          layers-data (assoc-node-to-layer layers-data layer nid)
          children (in-children graph nid)
          [tree added] (add-to-tree tree nid children added)]
      [(concat nextborder children) (conj visited nid) added tree layers-data])))

(defn build-tree-helper
  [graph border visited added layer tree layers-data]
  (prn "f border =")
  (pprint border)
  (if (seq border)
    (let [[nextborder visited added tree layers-data]
          (reduce (fn [[nextborder visited added tree layers-data] nid]
                    (explore-border nid graph nextborder
                                    visited added layer tree layers-data))
                  [() visited added tree layers-data]
                  border)]
      (prn "nextborder =")
      (pprint nextborder)
      (recur graph nextborder visited added (inc layer) tree layers-data)
      )
    [tree layers-data]))

;; (defn label-sizes-helper
;;   [graph layers-data])

;; (defn label-sizes
;;   [graph layers-data]
  
;;   )

(defn build-tree
  [graph]
  (let [rootnode (find-root graph)
        tree (create-graph)
        [tree layers-data] (build-tree-helper graph
                                               [rootnode]
                                               #{}
                                               #{}
                                               0
                                               tree
                                               {:layers [] :nodes {}})]
    (prn "layers-data =")
    (pprint layers-data)
    (prn "tree =")
    (pprint tree)
    (prn "ok")
    (export (build (layout tree :naive)) "/tmp/tree.svg" :indent "yes")
    ))

(defrecord RadialLayout
    []
  Layout

  (layout-graph
   [this graph options]
   (let [[tree layers-data] (build-tree graph)]
     )
   graph
   ))

(defn radiallayout
  []
  (RadialLayout.))
