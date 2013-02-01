;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0


(ns lacij.layouts.utils.position
  (:use lacij.edit.graph
        lacij.view.core))

(defn make-graph-visible
  "Translates all nodes in the graph to make all nodes' coordinates positives."
  [graph]
  (let [ids (keys (:nodes graph))
        fnode ((:nodes graph) (first ids))
        xleft (-> fnode :view :x)
        yupper (-> fnode :view :y)
        [uppermost yupper leftmost xleft]
        (reduce (fn [[uppermost yupper leftmost xleft :as acc] id]
                  (let [view (:view ((:nodes graph) id))
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
      (reduce (fn [graph node]
                (let [view (:view node)
                      x (:x view)
                      y (:y view)
                      destx (+ x xtrans)
                      desty (+ y ytrans)]
                 (move-node graph (:id node) destx desty)))
              graph
              (vals (:nodes graph))))))

(defn best-node
  [graph valfn cmp]
  (reduce (fn [maxid id]
            (if (cmp (valfn id) (valfn maxid))
              id
              maxid))
          (keys (:nodes graph))))

(defn lowest-node
  [graph]
  (best-node graph
             (fn [id]
               (let [view (:view ((:nodes graph) id))
                     [_ y _ height] (bounding-box view)]
                 (+ y height)))
             >))

(defn rightest-node
  [graph]
  (best-node graph
             (fn [id]
               (let [view (:view ((:nodes graph) id))
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
        view (:view ((:nodes graph) low))
        [_ y _ h] (bounding-box view)
        ylow (+ y h)
        right (rightest-node graph)
        viewright (:view ((:nodes graph) right))
        [x _ w _] (bounding-box viewright)
        xright (+ x w)
        spacing 10]
    (assoc graph :width (round-up xright) :height (round-up ylow))))

(defn widest-node
  "Returns the node id of the widest node in the collection. "
  [graph nodes]
  (first (sort-by (fn [id] (:width (:view ((:nodes graph) id)))) > nodes)))

(defn widest-value
  "Returns the width of the widest node in the collection. "
  [graph nodes]
  (:width (:view ((:nodes graph) (widest-node graph nodes)))))
