;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the EdgeView protocol for SVG"}
  lacij.view.svg.edgeview
  (:use clojure.pprint
        tikkba.dom
        lacij.utils.core
        lacij.geom.distance
        lacij.graph.core
        lacij.view.core
        lacij.view.svg.utils.style)
  (:require [analemma.svg :as s]
            [tikkba.utils.dom :as dom]))

(defrecord SvgEdgeView
    [id labels style attrs]
  EdgeView

  (add-edge-label
   [this label]
   (update-in this [:labels] conj label))

  (edge-labels
    [this]
    labels)

  (view-edge
   [this graph edge context]
   (let [{:keys [doc]} context
         [x-src-port y-src-port x-dst-port y-dst-port] (edge-location this graph edge)
         texts (map (fn [label]
                      (let [text (edgelabel-text label)
                            position (edgelabel-position label)
                            style (edgelabel-style label)]
                        (-> (s/text  {:x (double (/ (+ x-src-port x-dst-port) 2))
                                      :y (double (/ (+ y-src-port y-dst-port) 2))
                                      :text-anchor "middle"}
                                     text)
                            (apply-styles {:dominant-baseline :central} style))))
                    labels)
         attrs2 (dissoc attrs :marker-end)
         xml (concat
              (s/group {:id (name id)}
               (-> (cond (= ::not-found (get attrs :marker-end ::not-found))
                         (s/line x-src-port y-src-port x-dst-port y-dst-port
                             :marker-end "url(#lacij-end-arrow-marker)")

                         (nil? (get attrs :marker-end ::not-found))
                         (s/line x-src-port y-src-port x-dst-port y-dst-port)

                         :else
                         (s/line x-src-port y-src-port x-dst-port y-dst-port
                                 :marker-end (:marker-end attrs)))
                   (apply-styles {:stroke "#000000" :stroke-width 1} style)
                   (apply-attrs attrs2)))
              texts)]
     (dom/elements doc svg-ns xml)
     ))

  (edge-location
   [this graph edge]
   (let [src-view (node-view (node graph (src edge)))
         src-ports (ports src-view)
         dst-view (node-view (node graph (dst edge)))
         dst-ports (ports dst-view)
         [x1 y1] (node-center src-view)
         [x2 y2] (node-center dst-view)
         dists
         (sort-by
          :dist
          (for [srcport src-ports
                dstport dst-ports]
            {:dist (+ (distance (first srcport) (second srcport)
                                (first dstport) (second dstport)))
             :src-port srcport
             :dst-port dstport}))
         shortest-dist (first dists)
         shortest-dists (get (group-by :dist dists) (:dist shortest-dist))]
     (if (= (count shortest-dists) 1)
       (concat (:src-port shortest-dist) (:dst-port shortest-dist))
       ;; if several edges have the same length, we take the one
       ;; that is closest to the center of the dst node
       (let [disttocenter (fn [dist]
                            (distance (first (:dst-port dist))
                                      (second (:dst-port dist))
                                      x2
                                      y2))
             shortest-dist (first (sort-by disttocenter shortest-dists))]
         (concat (:src-port shortest-dist) (:dst-port shortest-dist)))))))

(defn svgedgeview
  ([id]
     (SvgEdgeView. id [] {} {}))
  ([id style attrs]
     (SvgEdgeView. id [] style attrs)))