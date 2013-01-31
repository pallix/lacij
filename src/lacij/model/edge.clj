;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Edge record and functions."}
  lacij.model.edge)

(defrecord Edge
    [id view srcid dstid])

(defn create-edge
  [id view id-node-src id-node-dst]
  (Edge. id view id-node-src id-node-dst))




