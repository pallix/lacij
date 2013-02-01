;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeLabelView protocol"}
  lacij.view.nodelabelview
  (:use lacij.view.core))

(defrecord NodeLabelView
    [text position style attrs])

(defn create-nodelabelview
  ([text position]
     (NodeLabelView. text position {} {}))
  ([text position style attrs]
     (NodeLabelView. text position style attrs)))