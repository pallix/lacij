;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeView protocol for rectanguler nodes"}
  lacij.view.svg.rectnodeview
  (:use clojure.pprint
        lacij.view.core
        lacij.view.svg.utils.style
        lacij.view.svg.rectnodeselection
        tikkba.dom)
  (:require [analemma.svg :as s]
            [analemma.xml :as xml]
            [tikkba.utils.dom :as dom])
  (:import java.awt.Rectangle))

(defn by-two
  [i]
  (/ i 2))

(def *selection-decorator* (rectnodeselection))

(defrecord SvgRectNodeView
    [id
     x
     y
     width
     height
     labels
     default-style
     style
     attrs
     decorators]
  NodeView

  (node-center
   [this]
   [(double (+ x (by-two width)))
    (double (+ y (by-two height)))])

  (add-node-label
   [this label]
   (update-in this [:labels] conj label))

  (node-labels
   [this]
   labels)

  (view-node
   [this node context]
    (let [{:keys [doc tmpdecorators]} context
         [x-center y-center] (node-center this)
         xmargin 5
         ;; TODO: use the position indicator
         texts (map (fn [label]
                      (let [txt (text label)
                            pos (position label)
                            style (nodelabel-style label)
                            font-size (:font-size style)
                            dy (if (nil? font-size)
                                 15 ;; what is the default font-size?
                                 font-size)
                            text (if (string? txt)
                                   (s/text {:x (by-two width) :y (by-two height) :text-anchor "middle"} txt)
                                   (apply s/text {:text-anchor "start"}
                                          (map #(s/tspan {:dy dy :x xmargin} %) txt)))]
                        (apply-styles text {:dominant-baseline :central} style)))
                    labels)
         decorations (map #(decorate % this context) (concat decorators tmpdecorators))
         xml (concat (s/group
                      {:id (name id) :transform (format "translate(%s, %s)" x y)}
                      (-> [:rect {:height height :width width}]
                          (apply-styles default-style style)
                          (apply-attrs attrs)))
                     texts
                     decorations)]
      ;; (prn "xml =")
      ;; (pprint xml)
      (dom/elements doc *svg-ns* xml)))

  (ports
   [this]
   [[x y] [(double (+ x (by-two width))) y] [(+ x width) y]
    [x (double (+ y (by-two height)))] [(+ x width) (double (+ y (by-two height)))]
    [x (+ y height)] [(double (+ x (by-two width))) (+ y height)] [(+ x width) (+ y height)]])

  (add-node-styles-kv
   [this styles]
   (update-in this [:style] merge styles))

  (add-node-decorator
   [this decorator]
   (update-in this [:decorators] conj decorator))

  (remove-node-decorator
   [this decorator]
   (update-in this [:decorators] disj decorator))

  (node-decorators
    [this]
    decorators)

  (node-selection-decorator
   [this]
   *selection-decorator*
   )

  (contains-pt?
   [this x y]
   (.contains (Rectangle. (:x this) (:y this) width height) x y))

  (bounding-box
   [this]
   (let [margin 5]
     [(- x margin) (- y margin) (+ width (* 2 margin)) (+ height (* 2 margin))]))

  (node-width
   [this]
   width)

  (node-height
   [this]
   height)

  (node-x
   [this]
   x)

  (node-y
   [this]
   y)
)


(defn import-rect
  [xmlcontent infile-id id x y]
  (let [rect (first (xml/filter-xml xmlcontent [{:id (name infile-id)}]))
        attrs (second rect)
        {:keys [width height style]} attrs
        width (Double/parseDouble width)
        height (Double/parseDouble height)
        attrs (dissoc attrs :width :height :style :x :y :id)
        style (s/parse-inline-css style)]
    (SvgRectNodeView. id x y width height [] {} style attrs #{})))