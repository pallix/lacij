;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeLabelView protocol"}
  lacij.view.svg.nodelabelview
  (:use lacij.view.core))

(defrecord SvgNodeLabelView
    [text position style params]
  NodeLabelView

  (text
   [this]
   text)
  
  (position
   [this]
   position)

  (nodelabel-style
    [this]
    style))

(defn nodelabelview
  ([text position]
     (SvgNodeLabelView. text position {} {}))
  ([text position style params]
     (SvgNodeLabelView. text position style params)))