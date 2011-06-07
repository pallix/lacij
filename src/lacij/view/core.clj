;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Protocols definitions for the views."}
  lacij.view.core)

(defprotocol GraphView
  (view-graph [this graph options])
  (export-graph [this graph filename context options]))

(defprotocol NodeView
  (node-center [this])
  (add-node-label [this label])
  (node-labels [this])
  (view-node [this node context])
  (node-width [this])
  (node-height [this])
  (node-x [this])
  (node-y [this])
  (node-location [this])
  (ports [this])
  (add-node-attrs-kv [this attrs])
  (add-node-styles-kv [this styles])
  (add-node-decorator [this decorator])
  (remove-node-decorator [this decorator])
  (node-decorators [this])
  (node-selection-decorator [this])
  (contains-pt? [this x y])
  (bounding-box [this]))

(defprotocol EdgeView
  (view-edge [this graph edge context])
  (add-edge-label [this label])
  (edge-location [this graph edge]))

(defprotocol NodeLabelView
  (text [this])
  (position [this])
  (nodelabel-style [this])
  (nodelabel-position [this]))

(defprotocol EdgeLabelView
  (edgelabel-style [this])
  (edgelabel-text [this])
  (edgelabel-position [this]))

(defprotocol Decorator
  (decorate [this view context]))