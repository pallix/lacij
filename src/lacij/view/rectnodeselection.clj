;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Decorator for marking rectangular nodes as selected"}
  lacij.view.rectnodeselection
  (:use lacij.view.core)
  (:require [analemma.svg :as s]))

(defrecord RectNodeSelection
    []
  Decorator

  (decorate
   [this view context]
   (let [{:keys [x y width height]} view
         bordersize 2]
     (-> (s/rect (- x bordersize) (- y bordersize)
                 (+ height (* bordersize 2)) (+ width (* bordersize 2)) :id (str "lacij-selection-" (name (:id view))))
         (s/style :fill-opacity 0.0
                  :stroke :darkviolet
                  :stroke-width bordersize
                  :stroke-dasharray "4, 4")))))


