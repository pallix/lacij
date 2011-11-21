;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeView protocol for rectanguler nodes"}
  lacij.view.svg.rectnodeview
  (:use clojure.pprint
        lacij.utils.core
        lacij.view.core
        lacij.view.svg.rectnodeselection
        tikkba.dom
        [lacij.view.svg.utils style text])
  (:require [analemma.svg :as s]
            [analemma.xml :as xml]
            [tikkba.utils.dom :as dom])
  (:import java.awt.Rectangle))

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
          texts (view-labels labels {:x (by-two width)
                                     :y (by-two height)
                                     :xmargin 5
                                     :text-anchor "middle"
                                     :text-anchor-multi "start"})
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