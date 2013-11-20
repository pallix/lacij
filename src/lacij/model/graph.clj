;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Graph model."}
  lacij.model.graph
  (:use clojure.pprint
        (tikkba dom swing core)
        tikkba.apps.svgbrowser
        lacij.utils.core
        (lacij.model node edge history graph-history))
  (:require [tikkba.utils.dom :as dom])
  (:import (javax.swing.undo UndoManager UndoableEditSupport)))

(defrecord Graph
    [xmldoc
     width
     height
     viewBox
     svgcanvas
     nodes
     edges
     node-styles
     edge-styles
     node-attrs
     edge-attrs
     defs
     node-view-factory
     listeners
     nodes-selections
     undomanager
     undosupport
     history])

(def end-arrow-marker
  [:lacij-end-arrow-marker
   [:marker {:viewBox "0 0 10 10"
             :refX 10
             :refY 5
             :markerUnits "strokeWidth"
             :orient "auto"
             :markerWidth "14"
             :markerHeight "10"
             }
    [:polyline {:points "0,0 10,5 0,10 1,5"}]]])

(defn create-graph
  [& options]
  (let [{:keys [width height viewBox]
         :or {viewBox nil}} options
        xmldoc (dom/create-document (dom-implementation) svg-ns "svg" nil)
        svgcanvas (jsvgcanvas)
        undomanager (UndoManager.)
        undosupport (UndoableEditSupport.)]
    (.addUndoableEditListener undosupport undomanager)
    (Graph. xmldoc
            width
            height
            viewBox
            svgcanvas
            {} ;; nodes
            {} ;; edges
            {} ;; nodes' default styles
            {} ;; edges' default styles
            {} ;; nodes' default attributes
            {} ;; edges' default attributes
            [end-arrow-marker] ;; defs
            nil ;; factory
            (atom {}) ;; listeners
            #{} ;; nodes' selections
            undomanager
            undosupport
            (atom (simplehistory)))))

(defn in-children
  "Returns all src nodes of all in-edges for the node nid."
  [graph nid]
  (let [node ((:nodes graph) nid)]
    (map #(:src ((:edges graph) %)) (:inedges node))))

(defn out-children
  "Returns all dst nodes of all out-edges for the node nid."
  [graph nid]
  (let [node ((:nodes graph) nid)]
    (map #(:dst ((:edges graph) %)) (:outedges node))))

(defn inout-children
  "Returns all src nodes of all inout-edges for the node nid."
  [graph nid]
  (concat (in-children graph nid)
          (out-children graph nid)))

(defn find-roots
  "Returns a seq of the roots of the graph. The roots are the node
   with the minimum of out-edges"
  [graph]
  (let [allnodes (keys (:nodes graph))
        fnode (first allnodes)
        min (count (:outedges ((:nodes graph) fnode)))]
    (:roots
     (reduce
      (fn [{:keys [roots min] :as m} nodeid]
        (let [out (count (:outedges ((:nodes graph) nodeid)))]
          (cond
           ;; fewer out-edges than the current roots? discard them
           (< out min) (assoc m :roots #{nodeid} :min out)
           ;; the same number, add it (we have multiple roots)
           (= out min) (update-in m [:roots] conj nodeid)
           :else
           m)))
      {:roots #{fnode}
       :min min}
      allnodes))))
