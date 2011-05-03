;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeLabelView protocol"}
  lacij.view.svg.nodelabelview
  (:use lacij.view.core))

(defrecord SvgNodeLabelView
    [text position]
  NodeLabelView

  (text
   [this]
   text)
  
  (position
   [this]
   position)
  )

(defn nodelabelview
  [text position]
  (SvgNodeLabelView. text position))