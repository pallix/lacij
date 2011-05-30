;;; Copyright © 2010-2011 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0


(ns lacij.layouts.utils.position
  (:use lacij.graph.core
        lacij.view.core))

(defn make-graph-visible
  "Translates all nodes in the graph
   to make all nodes' coordinates positives."
  [graph]
  (let [ids (nodes graph)
        fnode (node graph (first ids))
        xleft (node-x (node-view fnode))
        yupper (node-y (node-view fnode))
        [uppermost yupper leftmost xleft]
        (reduce (fn [[uppermost yupper leftmost xleft :as acc] id]
                  (let [view (node-view (node graph id))
                        [xbox ybox _ _] (bounding-box view)]
                    ;; (printf "xbox = %s ybox = %s\n" xbox ybox)
                    (cond (and (< ybox yupper) (< xbox xleft))
                          [id ybox id xbox]

                          (< ybox yupper)
                          [id ybox leftmost xleft]

                          (< xbox xleft)
                          [uppermost yupper id xbox]

                          :else acc)))
                [fnode yupper fnode xleft]
                ids)
        xtrans (if (neg? xleft) (- xleft) 0)
        ytrans (if (neg? yupper) (- yupper) 0)]
    ;; (printf "xtrans = %s ytrans = %s\n" xtrans ytrans)
    (if (and (zero? xtrans) (zero? ytrans))
      graph
      (reduce (fn [graph nid]
                (let [view (node-view (node graph nid))
                      x (node-x view)
                      y (node-y view)
                      destx (+ x xtrans)
                      desty (+ y ytrans)]
                 (move-node graph nid destx desty)))
              graph
              (nodes graph)))))
