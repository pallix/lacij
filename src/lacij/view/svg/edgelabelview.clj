;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the EdgeLabelView protocol"}
  lacij.view.svg.edgelabelview
  (:use clojure.pprint
        lacij.view.core))

(defrecord SvgEdgeLabelView
    [text position style params]
  EdgeLabelView

  (edgelabel-text
   [this]
   text)
  
  (edgelabel-position
   [this]
   position)

  (edgelabel-style
   [this]
   style)
  )

(defn edgelabelview
  ([text position]
     (SvgEdgeLabelView. text position {} {}))
  ([text position style params]
     (SvgEdgeLabelView. text position style params)))