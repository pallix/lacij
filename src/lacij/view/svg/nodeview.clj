;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Factory to create different types of nodes"}
  lacij.view.svg.nodeview
  (:use lacij.utils.core
        lacij.view.svg.utils.style
        (lacij.view.svg rectnodeview circlenodeview))
  (:import lacij.view.svg.rectnodeview.SvgRectNodeView
           lacij.view.svg.circlenodeview.SvgCircleNodeView))

(defn nodeview
  [id shape x y style attrs]
  (let [default-style {:fill "white" :stroke "black"}
        {:keys [x y width height r] :or {x x y y width 100 height 40 r 20}} attrs]
    (condp = shape
        :rect (SvgRectNodeView. id x y width height [] default-style style attrs #{})
        :circle (SvgCircleNodeView. id x y r [] default-style style attrs #{})
      
        (SvgRectNodeView. id x y width height [] default-style style attrs #{}))))

