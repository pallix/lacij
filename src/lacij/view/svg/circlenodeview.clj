;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeView protocol for a circle"}
  lacij.view.svg.circlenodeview
  (:use clojure.pprint
        lacij.view.core
        lacij.view.svg.utils.style
        tikkba.dom)
  (:require [analemma.svg :as s]
            [analemma.xml :as xml]
            [tikkba.utils.dom :as dom])
  (:import java.lang.Math))

(defrecord SvgCircleNodeView
    [id x y radius labels default-style style attrs decorators]
  NodeView

  (node-center
   [this]
   [(+ x radius) (+ y radius)])

  (node-x
   [this]
   x)

  (node-y
   [this]
   y)

  (node-width
    [this]
    (* 2 radius))

  (node-height
    [this]
    (* 2 radius))

  (add-node-label
   [this label]
   (update-in this [:labels] conj label))

  (node-labels
   [this]
   labels)

  (view-node
   [this node context]
   (let [{:keys [doc]} context
         [xcenter ycenter] (node-center this)
         ;; TODO: use the position indicator
         texts (map (fn [label]
                      (let [txt (text label)
                            pos (position label)
                            style (nodelabel-style label)
                            font-size (:font-size style)
                            dy (if (nil? font-size)
                                 15 ;; what is the default font-size?
                                 font-size)]
                        (->
                         (if (string? txt)
                           (s/text {:x xcenter :y ycenter :text-anchor "middle"}
                                   txt)
                           (apply s/text {:x xcenter :y y :text-anchor "middle"}
                                  (map #(s/tspan {:dy dy :x xcenter} %) txt)))
                         (apply-styles {:dominant-baseline :central} style))))
                    labels)
         xml (concat (s/group
                      (-> (s/circle xcenter ycenter radius)
                          (apply-styles default-style style)
                          (apply-attrs (merge attrs {:id (name id)}))))
                     texts)]
     (dom/elements doc *svg-ns* xml)))

  (ports
   [this]
    (let [[xcenter ycenter] (node-center this)]
     (map (fn [angle]
            (let [ra (Math/toRadians angle)]
              [(double (+ xcenter (* radius (Math/cos ra))))
               (double (+ ycenter (* radius (Math/sin ra))))]))
          (range 0 360 22))))

  (add-node-decorator
   [this decorator]
   (update-in this [:decorators] conj decorator))

  (remove-node-decorator
   [this decorator]
   (update-in this [:decorators] disj decorator))

  (node-decorators
    [this]
    decorators)

  (bounding-box
   [this]
   (let [margin 5]
    [(- x margin) (- y margin) (+ (* 2 radius) (* 2 margin)) (+ (* 2 radius) (* 2 margin))])))

(defn import-circle
  [xmlcontent file-id id x y]
  (let [circle (first (xml/filter-xml xmlcontent [{:id (name file-id)}]))
        attrs (second circle)
        {:keys [r style]} attrs
        attrs (dissoc attrs :r :style :x :y :id :cx :cy)
        style (s/parse-inline-css style)
        r (Double/parseDouble r)]
    (SvgCircleNodeView. id x y r [] {} style attrs #{})))