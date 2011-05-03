;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Utilities functions to manipulate styles and attributes."}
  lacij.view.svg.utils.style
  (:use clojure.pprint)
  (:require [analemma.svg :as s]
            [analemma.xml :as xml]))

(defn apply-styles [tag default-style style]
  (let [styles (flatten (seq (merge default-style style)))]
    (apply s/style tag styles)))

(defn apply-attrs [tag attrs]
  (if (empty? attrs)
    tag
    (apply xml/add-attrs tag (flatten (seq attrs)))))


