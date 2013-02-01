;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeView protocol for rectanguler nodes"}
  lacij.view.rectnodeview
  (:use clojure.pprint
        lacij.utils.core
        lacij.view.core
        lacij.view.rectnodeselection
        tikkba.dom
        [lacij.view.utils style text])
  (:require [analemma.svg :as s]
            [analemma.xml :as xml]
            [tikkba.utils.dom :as dom])
  (:import java.awt.Rectangle))

(defrecord RectNodeView
    [id
     x
     y
     width
     height
     labels
     default-style
     style
     attrs
     decorators
     selection-decorator]
  NodeView

  (view-node
   [this node context]
    (let [{:keys [doc]} context
          [x-center y-center] (center this)
          texts (view-labels labels {:x (by-two width)
                                     :y (by-two height)
                                     :xmargin 5
                                     :text-anchor "middle"
                                     :text-anchor-multi "start"})
          decorations (map #(decorate % this context) decorators)
          xml (concat (s/group
                       {:id (name id) :transform (format "translate(%s, %s)" x y)}
                       (-> [:rect {:height height :width width}]
                           (apply-styles default-style style)
                           (apply-attrs attrs)))
                      texts
                      decorations)]
      ;; (prn "xml =")
      ;; (pprint xml)
      (dom/elements doc svg-ns xml)))

  (center
    [this]
    [(double (+ x (by-two width)))
     (double (+ y (by-two height)))])

  (ports
   [this]
   [[x y] [(double (+ x (by-two width))) y] [(+ x width) y]
    [x (double (+ y (by-two height)))] [(+ x width) (double (+ y (by-two height)))]
    [x (+ y height)] [(double (+ x (by-two width))) (+ y height)] [(+ x width) (+ y height)]])
  
  (contains-pt?
   [this x y]
   (.contains (Rectangle. (:x this) (:y this) width height) x y))

  (bounding-box
   [this]
   (let [margin 5]
     [(- x margin) (- y margin) (+ width (* 2 margin)) (+ height (* 2 margin))])))

(defn import-rect
  [xmlcontent infile-id id x y]
  (let [rect (first (xml/filter-xml xmlcontent [{:id (name infile-id)}]))
        attrs (second rect)
        {:keys [width height style]} attrs
        width (Double/parseDouble width)
        height (Double/parseDouble height)
        attrs (dissoc attrs :width :height :style :x :y :id)
        style (s/parse-inline-css style)]
    (RectNodeView. id x y
                   width height [] {} style attrs #{}
                   (->RectNodeSelection))))