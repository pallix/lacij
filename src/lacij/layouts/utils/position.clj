;;; Copyright © 2010-2011 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0


(ns lacij.layouts.utils.position
  (:use lacij.graph.core
        lacij.view.core))

(defn make-graph-visible
  "Translates all nodes in the graph to make all nodes' coordinates positives."
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

(defn best-node
  [graph val cmp]
  (reduce (fn [maxid id]
            (if (cmp (val id) (val maxid))
              id
              maxid))
          (nodes graph)))

(defn lowest-node
  [graph]
  (best-node graph
             (fn [id]
               (let [view (node-view (node graph id))
                     [_ y _ height] (bounding-box view)]
                 (+ y height)))
             >))

(defn rightest-node
  [graph]
  (best-node graph
             (fn [id]
               (let [view (node-view (node graph id))
                     [x _ width _] (bounding-box view)]
                 (+ x width)))
             >))

(defn round-up
  [x]
  (int (Math/ceil x)))

(defn adjust-size
  "adjusts the width and the height"
  [graph]
  (let [low (lowest-node graph)
        view (node-view (node graph low))
        [_ y _ h] (bounding-box view)
        ylow (+ y h)
        right (rightest-node graph)
        viewright (node-view (node graph right))
        [x _ w _] (bounding-box viewright)
        xright (+ x w)
        spacing 10]
    (assoc graph :width (round-up xright) :height (round-up ylow))))

