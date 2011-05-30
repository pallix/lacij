;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Protocols for the graph, node and edge models 
            as well as wrapper functions."}
  lacij.graph.core)

(defprotocol Graph
  (width [this])
  (height [this])
  (add-node-kv [this id params])
  (add-edge-kv [this id id-node-src id-node-dst params])
  (add-label-kv [this id label params])
  (add-default-node-style-kv [this node-styles])
  (add-default-node-attrs-kv [this node-attrs]) 
  (add-default-edge-style-kv [this edge-styles])
  (add-default-edge-attrs-kv [this edge-attrs])
  (add-def [this def])
  (node [this id])
  (nodes [this])
  (edge [this id])
  (edges [this])
  (build [this])
  (view [this])
  (export-kv [this filename options])
  (set-node-view-factory [this f])
  (set-node-view [this id view])
  (move-node [this id x y])
  (move-node-center [this id x y]))

(defprotocol DynamicGraph
  (add-node-kv! [this id params])
  (add-edge-kv! [this id id-node-src id-node-dst params])
  (add-node-styles-kv! [this id styles])
  (set-node-selected! [this id true])
  (add-listener-vec [this id type f args])
  (canvas [this]))

(defprotocol UndoableGraph
  (begin-update [this])
  (end-update [this])
  (can-undo? [this])
  (undo! [this])
  (can-redo? [this])
  (redo! [this]))

(defprotocol Node
  (node-view [this])
  (change-node-view [this view])
  (inout-edges [this])
  (in-edges [this])
  (out-edges [this]))

(defprotocol Edge
  (edge-view [this])
  (src [this])
  (dst [this]))

(defn x-add-node
  [f this id & params]
  (cond
   (empty? params)
   (f this id {})

   (keyword? (first params)) 
   (f this id (apply hash-map params))

   :else
   (let [[label x y & res] params
         x (or x 0)
         y (or y 0)]
     (f this id (merge {:label label :x x :y y}
                                 (apply hash-map res))))))

(defn add-node
  [this id & params]
  (apply x-add-node add-node-kv this id params))

(defn add-node!
  [this id & params]
  (apply x-add-node add-node-kv! this id params))

(defn x-add-edge
  [f this id src-id dst-id & params]
  (cond
   (empty? params)
   (f this id src-id dst-id {})

   (keyword? (first params))
   (f this id src-id dst-id (apply hash-map params))

   :else
   (let [[label & res] params]
     (f this id src-id dst-id (merge {:label label}
                                     (apply hash-map res))))
   ))

(defn add-edge
  [this id src-id dst-id & params]
  (apply x-add-edge add-edge-kv this id src-id dst-id params))

(defn add-edge!
  [this id src-id dst-id & params]
  (x-add-edge add-edge-kv! this id src-id dst-id params))

(defn add-label
  [this id label & params]
  (add-label-kv this id label (apply hash-map params)))

(defn add-default-node-style
  [this & params]
  (add-default-node-style-kv this (apply hash-map params)))

(defn add-default-edge-style
  [this & params]
  (add-default-edge-style-kv this (apply hash-map params)))

(defn add-default-node-attrs
  [this & params]
  (add-default-node-attrs-kv this (apply hash-map params)))

(defn add-default-edge-attrs
  [this & params]
  (add-default-edge-attrs-kv this (apply hash-map params)))

(defn add-listener
  [this id type f & args]
  (add-listener-vec this id type f args))

(defn add-node-styles!
  [this id & params]
  (add-node-styles-kv! this id (apply hash-map params)))

(defn export
  [this filename & options]
  (export-kv this filename (apply hash-map options)))

(defmacro do-update [graph & body]
  "Executes body inside an update"
  `(let [graph# ~graph]
     (try
       (begin-update graph#)
       ~@body
       (finally
        (end-update graph#)))))

(defn in-children
  "Returns all src nodes of all in-edges for the node nid."
  [graph nid]
  (let [n (node graph nid)]
   (concat (map #(src (edge graph %)) (in-edges n)))))

(defn find-root
  "Returns the root of the graph. The root is the node
   with the minimum of out-edges"
  [graph]
  (let [allnodes (nodes graph)]
    (first
     (reduce
      (fn [[rootid noutedges] nodeid]
        (let [out (count (out-edges (node graph nodeid)))]
          (if (< out noutedges)
            [nodeid out]
            [rootid noutedges])))
      [(first allnodes) (count (out-edges (node graph (first allnodes))))]
      allnodes))))

(def ^{:doc "Generates a unique id for an edge."} geneid (partial (comp keyword gensym) "e"))

(def ^{:doc "Generates a unique id for a node"} genid (partial (comp keyword gensym) "h"))
