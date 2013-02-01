;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the GraphView protocol for SVG"}
  lacij.view.graphview
  (:use clojure.pprint
        lacij.view.core
        (tikkba dom core))
  (:require [analemma.svg :as s]
            [tikkba.utils.dom :as dom]))

(defn view-graph
 [graph graph context]
 (let [defs (s/defs (apply concat (:defs context)))
       {:keys [doc width height viewBox]} context
       markers-def (dom/elements doc svg-ns defs)
       node-elements (map (fn [node]
                            (view-node (:view node) node context))
                          (vals (:nodes graph)))
       edge-elements (map (fn [edge]
                            (view-edge (:view edge) graph edge context))
                          (vals (:edges graph)))
       doc-element (dom/document-element doc)
       mastergroup-element (dom/elements doc svg-ns [:g {:id "graph0"}])]
   (dom/add-attrs doc-element :overflow "visible")
   (when width
     (dom/add-attrs doc-element :width width))
   (when height
     (dom/add-attrs doc-element :height height))
   (when viewBox
     (dom/add-attrs doc-element :viewBox viewBox))
   (dom/append-child doc-element markers-def)
   (dom/append-child doc-element mastergroup-element)
   (dom/append-children mastergroup-element edge-elements)
   (dom/append-children mastergroup-element node-elements)
   doc))

(defn export
 [graph filename & options]
 (apply dom/spit-xml filename (:xmldoc graph) options))
