;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the Graph protocol for SVG."}
  lacij.graph.svg.graph
  (:use clojure.pprint
        (tikkba dom swing core)
        tikkba.apps.svgbrowser
        lacij.utils.core
        lacij.graph.core
        (lacij.graph.svg node edge)
        lacij.view.core
        (lacij.view.svg graphview nodeview edgeview nodelabelview edgelabelview)
        (lacij.graph.svg graph-helpers history graph-history))
  (:require [tikkba.utils.dom :as dom])
  (:import (javax.swing.undo UndoManager UndoableEditSupport)))



(defrecord SvgGraph
    [xmldoc
     width
     height
     svgcanvas
     nodes
     edges
     graphview
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
     history]
  Graph
  DynamicGraph
  UndoableGraph

  (add-node-kv
   [this id params]
   (let [[node _] (create-node id params node-styles node-attrs node-view-factory)]
     (update-in this [:nodes] assoc id node)))
  
  (add-edge-kv
   [this id id-node-src id-node-dst params]
   (let [{:keys [label style]} params
         rest-params (dissoc params :label :style)
         edgeview (svgedgeview (merge edge-styles style) (merge edge-attrs rest-params))
         edgeview (if (nil? label)
                    edgeview
                    (add-edge-label edgeview (edgelabelview label :center)))
         edge (svgedge id edgeview id-node-src id-node-dst)
         graph (update-node-edges this id id-node-src id-node-dst)]
     (update-in graph [:edges] assoc id edge)))

  (add-label-kv
   [this id label params]
    (cond (contains? edges id)
          (let [edge (get edges id)
                edgeview (edge-view edge)
                edgeview (add-edge-label edgeview
                                         (edgelabelview label :center (:style params)
                                                        (dissoc params :style)))
                edge (svgedge id edgeview (src edge) (dst edge))]
            (update-in this [:edges] assoc id edge))

          (contains? nodes id)
          (let [node (get nodes id)
                nodeview (node-view node)
                nodeview (add-node-label nodeview
                                         (nodelabelview label :center
                                                        (:style params)
                                                        (dissoc params :style)))
                node (svgnode id nodeview)]
            (update-in this [:nodes] assoc id node))))

  (add-default-node-style-kv
   [this node-styles]
   (update-in this [:node-styles] merge node-styles))

  (add-default-node-attrs-kv
   [this node-attrs]
   (update-in this [:node-attrs] merge node-attrs))

  (add-default-edge-style-kv
   [this edge-styles]
   (update-in this [:edge-styles] merge edge-styles))

  (add-default-edge-attrs-kv
   [this edge-attrs]
   (update-in this [:edge-attrs] merge edge-attrs))

  (add-def
   [this def]
   (update-in this [:defs] conj def))

  (node
   [this id]
   (get nodes id))

  (edge
   [this id]
   (get edges id))

  (nodes
   [this]
   (keys nodes))

  (edges
   [this]
   (keys edges))

  (build
   [this]
   (view-graph graphview this {:defs defs :doc xmldoc :width width :height height})
   (set-document svgcanvas xmldoc)
   ;; set initial state:
   (swap! history add-state (graphstate this))
   this)
  
  (view
   [this]
   xmldoc)

  (canvas
   [this]
   svgcanvas)

  (export-kv
   [this filename options]
   (export-graph graphview this filename {:defs defs} options))

  (set-node-view-factory
   [this f]
   (assoc this :node-view-factory f))

  (set-node-view
   [this id view]
   (let [node (get nodes id)
         current-view (node-view node)
         labels (node-labels current-view)
         view (reduce (fn [view label]
                        (add-node-label view label))
                      view labels)
         node (change-node-view node view)]
     (update-in this [:nodes] assoc id node))
   )

  (move-node
   [this id x y]
   (let [node (get nodes id)
         current-view (node-view node)
         new-view (assoc current-view :x x :y y)
         node (change-node-view node new-view)]
     (update-in this [:nodes] assoc id node)))

  (add-listener-vec
   [this id type f args]
   (if-let [el (dom/element-id xmldoc id)]
     (do
       (apply dom/add-event-listener el type f args)
       (update-listeners this id type f args)
       this)
     this))
  
  (add-node-kv!
   [this id params]
   (let [[node node-view] (create-node id params node-styles node-attrs node-view-factory)
         node-element (view-node node-view node {:doc xmldoc})
         docel (dom/document-element xmldoc)
         edit (node-inserted-edit docel nil node-element)
         graph (update-in this [:nodes] assoc id node)
         edit2 (add-state-edit history (graphstate graph))
         compedit (compound-edit edit edit2)]
     (post-edit undosupport compedit)
     graph))

  (add-node-styles-kv!
   [this id styles]
   (if-let [node (get nodes id)]
     (let [nodeview (node-view node)
           nodeview (add-node-styles-kv nodeview styles)
           node (svgnode id nodeview)
           node-element (view-node nodeview node {:doc xmldoc})
           [graph edit] (replace-node this id node node-element)]
       (post-edit undosupport edit)
       graph)
     this))

  (add-edge-kv!
   [this id id-node-src id-node-dst params]
   (let [[edge edgeview] (create-edge id params id-node-src id-node-dst edge-styles edge-attrs)
         edge-element (view-edge edgeview this edge {:doc xmldoc})
         docel (dom/document-element xmldoc)
         edit (node-inserted-edit docel nil edge-element)
         graph (update-in this [:edges] assoc id edge)
         graph (update-node-edges graph id id-node-src id-node-dst)
         edit2 (add-state-edit history (graphstate graph))
         compedit (compound-edit edit edit2)]
     (post-edit undosupport compedit)
     graph))

  (can-undo?
   [this]
   (.canUndo undomanager))

  (undo!
   [this]
   (if (can-undo? this)
     (do
       (.undo undomanager)
       (let [graph (restore-state this (current-state (deref history)))
             graph (refresh-nodes-selections graph
                                             (:nodes-selections this)
                                             (:nodes-selections graph))]
         graph))
     this))

  (can-redo?
   [this]
   (.canRedo undomanager))

  (redo!
   [this]
   (if (can-redo? this)
     (do
       (.redo undomanager)
       (let [graph (restore-state this (current-state (deref history)))
             graph (refresh-nodes-selections graph
                                             (get this :nodes-selections)
                                             (:nodes-selections graph))]
         (swap! (:history graph) update-current-state (graphstate graph))
         graph))
     this))

  (begin-update
   [this]
   (.beginUpdate undosupport))

  (end-update
   [this]
   (.endUpdate undosupport))

  (set-node-selected!
   [this id selected]
   (let [current-selected (conj (get this :nodes-selections) id)
         graph (refresh-nodes-selections this
                                         (get this :nodes-selections)
                                         current-selected)]
     (swap! history update-current-state (graphstate graph))
     graph))

    (width
     [this]
     width)
    
    (height
     [this]
     height))

(def end-arrow-marker
  [:lacij-end-arrow-marker
   [:marker {:viewBox "0 0 10 10"
             :refX 8
             :refY 5
             :markerUnits "strokeWidth"
             :orient "auto"
             :markerWidth "14"
             :markerHeight "10"
             }
    [:polyline {:points "0,0 10,5 0,10 1,5"}]]])

(defn create-graph
  [& options]
  (let [{:keys [width height]} options
        view (graphview)
        xmldoc (dom/create-document (dom-implementation) *svg-ns* "svg" nil)
        svgcanvas (jsvgcanvas)
        undomanager (UndoManager.)
        undosupport (UndoableEditSupport.)]
    (.addUndoableEditListener undosupport undomanager)
    (SvgGraph. xmldoc
               width
               height
               svgcanvas
               {} ;; nodes
               {} ;; edges
               view
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

(defmacro do-batik-update
  "Executes body inside a batik thread and insides an update"
  [graph & body]
  `(let [graph# ~graph
         svgcanvas# (canvas graph#)]
     (do-batik
      svgcanvas#
      (do-update
        graph#
        ~@body))))