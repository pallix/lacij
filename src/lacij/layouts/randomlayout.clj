;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "This layout spreads nodes randomly
            but still avoid overlapping."}
  lacij.layouts.randomlayout
  (:use lacij.edit.graph
        lacij.geom.intersect
        lacij.layouts.core
        lacij.view.core))

(defrecord RandomLayout
    []
  Layout
  
  (layout-graph
   [this graph options]
   (letfn [(collision?
            [graph id]
             (let [view (:view ((:nodes graph) id))
                   nodeviews (map :view (vals (:nodes graph)))]
              (some #(and (not= % view)
                          (apply rect-intersects?
                                 (concat (bounding-box view)
                                         (bounding-box %))))
                    nodeviews)))]
     (let [{:keys [width height]
            :or {width (:width graph) height (:height graph)}} options
            width (if (nil? width) 1024 width)
            height (if (nil? height) 768 height)]
       (reduce
        (fn [graph id]
          (first
           (filter
            #(not (collision? % id))
            (map (fn [[x y]]
                   (move-node graph id x y))
                 (repeatedly (fn []
                               [(rand-int width) (rand-int height)]))))))
        graph
        (keys (:nodes graph)))))))

(defn randomlayout []
  (RandomLayout.)) 
