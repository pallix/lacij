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

(defn count-out-edges
  [graph nid]
  (count (:outedges ((:nodes graph) nid))))

(defn count-in-edges
  [graph nid]
  (count (:inedges ((:nodes graph) nid))))

(defn orphan?
  "Returns true if the node is an orphan."
  [graph nid]
  (and (zero? (count-out-edges graph nid))
       (zero? (count-in-edges graph nid))))

(defn find-roots
  "Returns a seq of the roots of the graph. The roots are the nodes with the
   minimum number of out-edges. Orphan nodes are ignored."
  [graph flow]
  (let [allnodes (keys (:nodes graph))
        fnode (first (filter (complement (partial orphan? graph))
                             allnodes))
        count-fn (if (= flow :in)
                   count-out-edges
                   count-in-edges)
        min (count-fn graph fnode)]
    (if-not fnode
      ()
      (seq (:roots
            (reduce
             (fn [{:keys [roots min] :as m} nodeid]
               (let [out (count-fn graph nodeid)]
                 (cond
                  ;; fewer out-edges than the current roots? discard them
                  (and (< out min) (not (orphan? graph nodeid))) (assoc m :roots #{nodeid} :min out)
                  ;; the same number, add it (we have multiple roots)
                  (= out min) (update-in m [:roots] conj nodeid)
                  :else
                  m)))
             {:roots #{fnode}
              :min min}
             allnodes))))))

(defn depth-first-node
  [graph node get-children {:keys [visited ordered-visited] :as context}]
  (let [ordered-visited (conj ordered-visited node)
        visited (conj visited node)
        context (assoc context :visited visited :ordered-visited ordered-visited)
        non-visited-children (filter (complement visited) (get-children node))]
    (reduce (fn [context node]
              (if ((:visited context) node)
                context
                (depth-first-node graph node get-children context)))
            context
            non-visited-children)))

(defn depth-first
  "Returns a depth-first sequence of the nodes in the graph.
The roots used to begin the search are defined by the user. "
  [graph get-children roots]
  (:ordered-visited
   (reduce (fn [context node]
             (if ((:visited context) node)
               context
               (depth-first-node graph node get-children context)))
           {:visited #{}
            :ordered-visited []}
           roots)))
