;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the Edge protocol for SVG."}
  lacij.graph.svg.edge
  (:use lacij.graph.core))

(defrecord SvgEdge
    [id view srcid dstid]
  Edge

  (edge-view
   [this]
   view)

  (src
   [this]
   srcid)

  (dst
   [this]
   dstid)
  )

(defn svgedge
  [id view id-node-src id-node-dst]
  (SvgEdge. id view id-node-src id-node-dst))




