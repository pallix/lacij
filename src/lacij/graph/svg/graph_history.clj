;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Functions for the implementation of undo/redo"}
  lacij.graph.svg.graph-history
  (:use clojure.pprint
        clojure.set
        lacij.graph.svg.history
        tikkba.apps.svgbrowser)
  (:require [tikkba.utils.dom :as dom]))

(defrecord GraphState
    [map])

(defn graphstate
  [graph]
  ;; store all immutable fields
  (let [keys-to-store (difference (set (keys graph))
                                  #{:xmldoc
                                    :svgcanvas
                                    :undomanager
                                    :undosupport
                                    :history})
        state (GraphState. (select-keys graph keys-to-store))]
    state))

(defn restore-state
  [graph state]
  (merge graph (:map state)))

(defn add-state-edit
  [atom-history newstate]
  (letfn [(todo
           []
           (swap! atom-history add-state newstate))

          (toundo
           []
           (swap! atom-history undo-state))

          (toredo
           []
           (swap! atom-history redo-state))]
    (create-edit todo toundo toredo)))

(defn attach-listeners-edit
  [xmldoc id listeners]
  (letfn [(todo
           []
           (let [el (dom/element-id xmldoc id)]
             (doseq [[type f args] listeners]
               (apply dom/add-event-listener el type f args))))

          (toundo
           []
           ;; TODO
           )]
    (create-edit todo toundo todo)))