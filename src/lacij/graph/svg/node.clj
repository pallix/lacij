;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the node protocol for SVG."}
  lacij.graph.svg.node
  (:use lacij.graph.core))

(defrecord SvgNode
    [id view inedges outedges]
  Node

  (node-view
   [this]
   view)

  (change-node-view
   [this view]
   (assoc this :view view))

  (in-edges
   [this]
   inedges)

  (out-edges
   [this]
   outedges)

  (inout-edges
   [this]
   (apply conj (in-edges this) (out-edges this))))

(defn svgnode
  [id view]
  (SvgNode. id view #{} #{}))