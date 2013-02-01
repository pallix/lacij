;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeView protocol for a circle"}
  lacij.view.circlenodeview
  (:use clojure.pprint
        lacij.utils.core
        lacij.view.core
        lacij.view.utils.style
        tikkba.dom
        [lacij.view.utils text])
  (:require [analemma.svg :as s]
            [analemma.xml :as xml]
            [tikkba.utils.dom :as dom])
  (:import java.lang.Math))

 
(defrecord CircleNodeView
    [id
     x
     y
     radius
     labels
     default-style
     style
     attrs
     decorators]
  NodeView

  (view-node
   [this node context]
   (let [{:keys [doc]} context
         texts (view-labels labels {:text-anchor "middle"
                                    :text-anchor-multi "middle"
                                    :x radius
                                    :y radius
                                    :y-multi (by-two radius)
                                    :xmargin radius})
         decorations (map #(decorate % this context) decorators)
         xml (concat (s/group
                      {:id (name id) :transform (format "translate(%s, %s)" x y)}
                      (-> [:circle {:r radius :cx radius :cy radius}]
                          (apply-styles default-style style)
                          (apply-attrs attrs)))
                     texts
                     decorations)]
     (dom/elements doc svg-ns xml)))

  (center
    [this]
    [(+ x radius) (+ y radius)])

  (ports
   [this]
    (let [[xcenter ycenter] (center this)]
     (map (fn [angle]
            (let [ra (Math/toRadians angle)]
              [(double (+ xcenter (* radius (Math/cos ra))))
               (double (+ ycenter (* radius (Math/sin ra))))]))
          (range 0 360 22))))

  (bounding-box
   [this]
   (let [margin 5]
     [(- x margin) (- y margin)
      (+ (* 2 radius) (* 2 margin)) (+ (* 2 radius) (* 2 margin))])))

(defn import-circle
  [xmlcontent file-id id x y]
  (let [circle (first (xml/filter-xml xmlcontent [{:id (name file-id)}]))
        attrs (second circle)
        {:keys [r style]} attrs
        attrs (dissoc attrs :r :style :x :y :id :cx :cy)
        style (s/parse-inline-css style)
        r (Double/parseDouble r)]
    (CircleNodeView. id x y r [] {} style attrs #{})))