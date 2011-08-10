;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the EdgeView protocol for segmented edges"}
  lacij.view.svg.segmentededgeview
  (:use clojure.pprint
        tikkba.dom
        lacij.geom.distance
        lacij.graph.core
        lacij.view.core
        lacij.view.svg.utils.style)
  (:require [analemma.svg :as s]
            [analemma.xml :as xml]
            [tikkba.utils.dom :as dom]))

(defn- build-path
  [xsrc ysrc xdst ydst points]
  (let [path [:M [xsrc ysrc]]
        path (reduce (fn [path point]
                       (let [path (conj path :L)
                             path (conj path point)
                             path (conj path :M)
                             path (conj path point)]
                         path))
                     path
                     points)
        ;; bug in firefox 6.0: marker + path not working
        ;; we need to draw a line at the end
        ;; path (conj path :L)
        ;; path (conj path [xdst ydst])
        ;; path (conj path :M)
        ;; path (conj path [xdst ydst])
        path (conj path :Z)
        path (conj path [])]
    path))

(defn- add-marker
  [line marker attrs]
  (cond (= ::not-found marker)
        (xml/add-attrs line :marker-end "url(#lacij-end-arrow-marker)")
        
        (nil? marker)
        line

        :else
        (xml/add-attrs line :marker-end (:marker-end attrs))))

(defn- draw-path
  [xsrc ysrc xdst ydst points style attrs]
  (let [marker (get attrs :marker-end ::not-found)
        attrs2 (dissoc attrs :marker-end)
        default {:stroke "#000000" :stroke-width 1}]
   (if (empty? points)
     (-> (s/line xsrc ysrc xdst ydst)
         (add-marker marker attrs)
         (apply-styles default style)
         (apply-attrs attrs2))
     (let [p (build-path xsrc ysrc xdst ydst points)
           lastx (first (last points))
           lasty (second (last points))]
       (s/group
        (-> (s/path p)
            (apply-styles default style)
            (apply-attrs attrs2))
        (-> (s/line lastx lasty xdst ydst)
            (add-marker marker attrs)
            (apply-styles default style)
            (apply-attrs attrs2)))))))

(defrecord SvgSegmentedEdgeView
    [labels style attrs points]
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
               (draw-path x-src-port y-src-port x-dst-port y-dst-port points style attrs))
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
     (concat (:src-port shortest-dist) (:dst-port shortest-dist)))))

(defn svgsegmentededgeview
  ([style attrs points]
     (SvgSegmentedEdgeView. [] style attrs points)))