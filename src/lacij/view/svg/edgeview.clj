;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the EdgeView protocol for SVG"}
  lacij.view.svg.edgeview
  (:use clojure.pprint
        tikkba.dom
        lacij.geom.distance
        lacij.graph.core
        lacij.view.core
        lacij.view.svg.utils.style)
  (:require [analemma.svg :as s]
            [tikkba.utils.dom :as dom]))

(defrecord SvgEdgeView
    [labels style attrs]
  EdgeView

  (add-edge-label
   [this label]
   (update-in this [:labels] conj label))

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
         xml (concat
              (s/group
               (-> (s/line  x-src-port y-src-port x-dst-port y-dst-port
                            :marker-end "url(#lacij-end-arrow-marker)")
                   (apply-styles {:stroke "#000000" :stroke-width 1} style)
                   (apply-attrs attrs)))
              texts)]
     (dom/elements doc *svg-ns* xml)
     ))

  (edge-location
   [this graph edge]
   (let [src-view (node-view (node graph (src edge)))
         src-ports (ports src-view)
         dst-view (node-view (node graph (dst edge)))
         dst-ports (ports dst-view)
         [x1-line y1-line] (node-center src-view)
         [x2-line y2-line] (node-center dst-view)
         shortest-dist (first
                        (sort-by
                         :dist
                         (for [x src-ports
                               y dst-ports]
                           {:dist (+ (distance (first x) (second x)
                                               (first y) (second y))
                                     (distance (first x) (second x) x1-line y1-line)
                                     (distance (first y) (second y) x2-line y2-line))
                            :src-port x
                            :dst-port y})))]
     (concat (:src-port shortest-dist) (:dst-port shortest-dist))))
  
  )

(defn svgedgeview
  ([]
     (SvgEdgeView. [] {} {}))
  ([style attrs]
     (SvgEdgeView. [] style attrs)))