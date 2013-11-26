;;; Copyright © 2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Breaking graph cycles.

Implementation based on the S1 step of the algorithm describe in:

How to draw a directed graph by Peter Eades, Kozo Sugiyama
Journal of Information Processing (1990) Volume: 13, Issue: 4,
Publisher: IEEE, Pages: 13-17"}
  lacij.layouts.utils.cycle
  (:require [lacij.model.graph :as g]
            [lacij.edit.graph :as edit]))

(defn up-edge?
  [graph labeling flow edge-id]
  (let [edge ((:edges graph) edge-id)
        src (:src edge)
        dst (:dst edge)]
    (or (and (= flow :in)
             (> (labeling src) (labeling dst)))
        (and (= flow :out)
             (< (labeling src) (labeling dst))))))

(defn flip-edge
  [graph edge-id]
  (let [edge ((:edges graph) edge-id)
        {:keys [labels style attrs src dst]} edge
        graph (edit/remove-edge-by-id graph edge-id)
        graph (edit/add-edge-kv graph (:id edge) dst src {:labels labels :style style :attrs attrs})]
    graph))

(defn flip-edges
  [graph edges-ids]
  (reduce flip-edge graph edges-ids))

(defn dfs-break-cycles
  "Breaks the cycles contained in a graph by flipping edges.
Returns [graph flipped-edges]. "
  [graph flow]
  (let [get-children (if (= flow :in)
                       (partial g/in-children graph)
                       (partial g/out-children graph))
        nodes (g/depth-first graph get-children (concat (g/find-roots graph) (keys (:nodes graph))))
        labeling (zipmap nodes (iterate inc 0))
        up-edges (filter (partial up-edge? graph labeling flow) (keys (:edges graph)))
        graph (flip-edges graph up-edges)]
    [graph (set up-edges)]))

(defn greedy-break-cycles
  "Breaks the cycles contained in a graph by flipping edges.
Returns [graph flipped-edges]. "
  [graph flow]
  (let [ordered (sort-by (if (= flow :in)
                           (partial g/count-in-edges graph)
                           (partial g/count-out-edges graph))
                         (keys (:nodes graph)))
        labeling (zipmap ordered (iterate inc 0))
        up-edges (filter (partial up-edge? graph labeling flow) (keys (:edges graph)))
        graph (flip-edges graph up-edges)]
    [graph (set up-edges)]))

(def break-cycles greedy-break-cycles)
