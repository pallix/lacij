;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Various functions to manipulate the graph model"}
  lacij.model.graph-helpers
  (:use (tikkba dom swing core)
        tikkba.apps.svgbrowser
        lacij.utils.core
        lacij.model.core
        (lacij.model edge graph-history)
        lacij.view.core
        (lacij.view graphview nodeview edgeview nodelabelview edgelabelview
                    rectnodeview circlenodeview))
  (:require [tikkba.utils.dom :as dom]
            [lacij.model.node :as node]))



(defn create-edge
  [id params id-node-src id-node-dst edge-styles edge-attrs]
  (let [{:keys [label style]} params
        rest-params (dissoc params :label :style)
        edgeview (svgedgeview id (merge edge-styles style) (merge edge-attrs rest-params))
        edgeview (if (nil? label)
                   edgeview
                   (add-edge-label edgeview (edgelabelview label :center)))
        edge (svgedge id edgeview id-node-src id-node-dst)]
    [edge edgeview]))



(defn listeners-id [graph id]
  (get (deref (:listeners graph)) id))

(defn update-listeners
  [graph id type f args]
  (let [el-listeners (listeners-id graph id)
        el-listeners (if (nil? el-listeners) #{} el-listeners)
        el-listeners (conj el-listeners [type f args])]
    (swap! (:listeners graph) assoc id el-listeners)
    graph
    ;; (assoc-in graph [:listeners id] el-listeners)
    ))

(defn replace-node
  [graph id new-node new-el]
  (let [xmldoc (:xmldoc graph)
        history (:history graph)
        el (dom/element-id xmldoc id)
        graphel (dom/element-id xmldoc :graph0)
        docel (dom/document-element xmldoc)
        listeners (listeners-id graph id)
        removeedit (node-removed-edit graphel (dom/next-sibling el) el)
        insertedit (node-inserted-edit graphel nil new-el)
        graph (update-in graph [:nodes] assoc id new-node)
        graph (reduce (fn [graph [type f args]]
                        (update-listeners graph id type f args)) graph listeners)
        listenersedit (attach-listeners-edit xmldoc id listeners)
        historyedit (add-state-edit history (graphstate graph))
        compedit (compound-edit removeedit insertedit listenersedit historyedit)]
    [graph compedit]))

(defn refresh-nodes-selections
  [graph previous-selected current-selected]
  (let [xmldoc (:xmldoc graph)
        nodesselections (:node-selections graph)
        docelement (dom/document-element xmldoc)
        to-disable (clojure.set/difference previous-selected current-selected)
        to-enable (clojure.set/difference current-selected previous-selected)
        graph (assoc graph :nodes-selections current-selected)]
    (doseq [id to-disable]
      (let [xmlid (str "lacij-selection-" (name id))
            el (dom/element-by-id xmldoc xmlid)]
        (dom/remove-child docelement el)))
    (doseq [id to-enable]
      (let [nodeview (node-view (node graph id))
            selectiondecorator (node-selection-decorator nodeview)
            xml (decorate selectiondecorator nodeview {})
            el (dom/elements xmldoc svg-ns xml)]
        (dom/append-child docelement el)))
    graph))

(defn remove-edge-by-id
  [this id]
  (let [e (edge this id)
        s (src e)
        d (dst e)
        graph this
        graph (update-in graph [:edges] dissoc id)
        graph (update-in graph [:nodes s :outedges] disj id)
        graph (update-in graph [:nodes d :inedges] disj id)]
    graph))