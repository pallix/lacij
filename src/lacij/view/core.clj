;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Protocols definitions for the views."}
  lacij.view.core)

(defprotocol NodeView
  (add-node-label [this label])
  (node-labels [this])
  (view-node [this node context])
  (ports [this] "Returns the ports. Ports are points where the edges connections occurs.")
  (contains-pt? [this x y])
  (bounding-box [this]))

(defprotocol EdgeView
  (view-edge [this graph edge context])
  (edge-location [this graph edge]))

(defprotocol Decorator
  (decorate [this view context]))

(defn add-view-label
   [view label]
   (update-in view [:labels] conj label))

