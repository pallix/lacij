;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Dynamic modifications to the DOM operations."}
  lacij.edit.dynamic
  (:use lacij.edit.graph
        lacij.view.core
        lacij.model.history
        [lacij.model.graph-history :only [add-state-edit
                                          attach-listeners-edit
                                          restore-state]]
        lacij.view.core
        [lacij.view.straightedgeview :only [create-straight-edgeview]]
        [lacij.view.edgelabelview :only [create-edgelabelview]]
        [tikkba.dom :only [svg-ns]]
        [tikkba.apps.svgbrowser :only [node-inserted-edit
                                       node-removed-edit
                                       compound-edit
                                       post-edit]]
        [tikkba.utils.selection :as sel])
  (:require [lacij.model.node :as n]
            [lacij.model.edge :as e]
            [tikkba.utils.dom :as dom]))

(defn add-node-kv!
 [graph id params]
 (let [[node node-view] (create-node id params
                                     (:node-styles graph)
                                     (:node-attrs graph)
                                     (:node-view-factory graph))
       node-element (view-node node-view node {:doc (:xmldoc graph)})
       docel (dom/document-element (:xmldoc graph))
       edit (node-inserted-edit docel nil node-element)
       graph (update-in graph [:nodes] assoc id node)
       edit2 (add-state-edit (:history graph) (:graphstate graph))
       compedit (compound-edit edit edit2)]
   (post-edit (:undosupport graph) compedit)
   graph))


(defn create-edge
  [id params id-node-src id-node-dst edge-styles edge-attrs]
  (let [{:keys [label style]} params
        rest-params (dissoc params :label :style)
        edgeview (create-straight-edgeview id
                                           (merge edge-styles style)
                                           (merge edge-attrs rest-params))
        edgeview (if (nil? label)
                   edgeview
                   (update-in edgeview [:labels] conj (create-edgelabelview label :center)))
        edge (e/create-edge id edgeview id-node-src id-node-dst)]
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

(defn add-listener-vec
 [graph id type f args]
 (if-let [el (dom/element-id (:xmldoc graph) id)]
   (do
     (apply dom/add-event-listener el type f args)
     (update-listeners graph id type f args)
     graph)
   graph))

(defn add-listener
  [this id type f & args]
  (add-listener-vec this id type f args))

(defn- replace-node
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
        historyedit (add-state-edit history (:graphstate graph))
        compedit (compound-edit removeedit insertedit listenersedit historyedit)]
    [graph compedit]))

(defn- refresh-nodes-selections
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
      (let [nodeview (:view ((:nodes graph) id))
            selectiondecorator (:selection-decorator nodeview)
            xml (decorate selectiondecorator nodeview {})
            el (dom/elements xmldoc svg-ns xml)]
        (dom/append-child docelement el)))
    graph))

(defn add-node-styles-kv!
 [graph id styles]
 (if-let [node ((:nodes graph) id)]
   (let [nodeview (:view node)
         nodeview (update-in nodeview [:style] merge styles)
         node (n/create-node id nodeview)
         node-element (view-node nodeview node {:doc (:xmldoc graph)})
         [graph edit] (replace-node graph id node node-element)]
     (post-edit (:undosupport graph) edit)
     graph)
   graph))

(defn add-node-styles!
  [this id & params]
  (add-node-styles-kv! this id (apply hash-map params)))

(defn add-edge-kv!
 [graph id id-node-src id-node-dst params]
 (let [[edge edgeview] (create-edge id params id-node-src id-node-dst
                                    (:edge-styles graph)
                                    (:edge-attrs graph))
       edge-element (view-edge edgeview graph edge {:doc (:xmldoc graph)})
       docel (dom/document-element (:xmldoc graph))
       edit (node-inserted-edit docel nil edge-element)
       graph (update-in graph [:edges] assoc id edge)
       graph (update-node-edges graph id id-node-src id-node-dst)
       edit2 (add-state-edit (:history graph) (:graphstate graph))
       compedit (compound-edit edit edit2)]
   (post-edit (:undosupport graph) compedit)
   graph))

(defn add-edge!
  [this id src-id dst-id & params]
  (x-add-edge add-edge-kv! this id src-id dst-id params))

(defn add-node!
  [this id & params]
  (apply x-add-node add-node-kv! this id params))

(defn can-undo?
 [graph]
 (.canUndo (:undomanager graph)))

(defn undo!
 [graph]
 (if (can-undo? graph)
   (do
     (.undo (:undomanager graph))
     (let [graph (restore-state graph (current-state (deref (:history graph))))
           graph (refresh-nodes-selections graph
                                           (:nodes-selections graph)
                                           (:nodes-selections graph))]
       graph))
   graph))

(defn can-redo?
 [graph]
 (.canRedo (:undomanager graph)))

(defn redo!
 [graph]
 (if (can-redo? graph)
   (do
     (.redo (:undomanager graph))
     (let [graph (restore-state graph (current-state (deref (:history graph))))
           graph (refresh-nodes-selections graph
                                           (get graph :nodes-selections)
                                           (:nodes-selections graph))]
       (swap! (:history graph) update-current-state (:graphstate graph))
       graph))
   graph))

(defn set-node-selected!
 [graph id selected]
 (let [current-selected (conj (get graph :nodes-selections) id)
       graph (refresh-nodes-selections graph
                                       (get graph :nodes-selections)
                                       current-selected)]
   (swap! (:history graph) update-current-state (:graphstate graph))
   graph))

(defn clean!
  [graph]
  (let [root (.getDocumentElement (:xmldoc graph))]
    (doseq [node (dom/child-nodes-seq root)]
      (dom/remove-child root node))))


(defn autobox-texts!
  [graph]
  (let [groups (sel/selection-seq
                (.getDocumentElement
                 (:xmldoc graph))
                "g[class=rectangle-node]")
        graph (reduce (fn [graph g]
                        (let [box (.getBBox g)
                              id (keyword (.getId g))
                              width (Math/ceil (.getWidth box))
                              height (Math/ceil (.getHeight box))]
                          (update-node graph id :width width :height height)))
                      graph
                      groups)]
    (clean! graph)
    (add-node-kv! graph :xyz {:x 100 :y 100 :width 40 :height 40 :style {:fill "black"}})
    (build graph)))
