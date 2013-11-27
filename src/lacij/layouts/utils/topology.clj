;;; Copyright © 2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns lacij.layouts.utils.topology
  (:require [clojure.set :as set]
            [lacij.model.graph :refer :all]))

(defn topological-seq
  "Returns a topological sort of the nodes in the graph"
  [graph flow]
  ;; see https://en.wikipedia.org/wiki/Topological_sorting
  (let [children-fn (if (= flow :in)
                      in-children
                      out-children)
        s (filter (fn [n] (empty? (children-fn graph n))) (keys (:nodes graph)))]
    (loop [state {:s s
                  :l []
                  :removed #{}}]
      (if (empty? (:s state))
        (if-not (= (:removed state) (set (keys (:edges graph))))
          (throw (ex-info "Acyclic graph are not allowed" {}))
          (reverse (:l state)))
        (let [[node & remaining] (:s state)]
          (let [state (assoc state :s remaining)
                state (assoc state :l (cons node (:l state)))
                outedges (set/difference (set (if (= flow :in)
                                                (:outedges ((:nodes graph) node))
                                                (:inedges ((:nodes graph) node))))
                                         (:removed state))
                state (reduce (fn [state edge]
                                (let [state (update-in state [:removed] conj edge)
                                      dst (if (= flow :in)
                                            (:dst ((:edges graph) edge))
                                            (:src ((:edges graph) edge)))
                                      incoming (if (= flow :in)
                                                 (:inedges ((:nodes graph) dst))
                                                 (:outedges ((:nodes graph) dst)))
                                      incoming (set/difference (set incoming) (:removed state))]
                                  (if (empty? incoming)
                                    (update-in state [:s] conj dst)
                                    state)))
                              state
                              outedges)]
            (recur state)))))))

(defn has-cycle?
  [graph flow]
  (try (topological-seq graph flow)
       false
       (catch Exception _ true)))
