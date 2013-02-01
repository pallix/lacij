;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Node record and functions."}
  lacij.model.node)

(defrecord Node
    [id view inedges outedges])

(defn inout-edges
  [node]
  (into (:inedges node) (:outedges node)))

(defn create-node
  [id view]
  (Node. id view #{} #{}))