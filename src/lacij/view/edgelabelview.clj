;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the EdgeLabelView protocol"}
  lacij.view.edgelabelview
  (:use clojure.pprint
        lacij.view.core))

(defrecord EdgeLabelView
    [text position style params])

(defn create-edgelabelview
  ([text position]
     (EdgeLabelView. text position {} {}))
  ([text position style params]
     (EdgeLabelView. text position style params)))