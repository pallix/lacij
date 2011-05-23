;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the radial layout described in
 
            ﻿Wills, Graham J. 1999. 
           'NicheWorks: Interactive Visualization of Very Large Graphs.' 
            Journal of Computational and Graphical Statistics 8(2): 190.

            This layout looks well on graphs that are closed
            to a tree in their topology."}
  lacij.layouts.radiallayout
  (:use clojure.pprint
        clojure.contrib.trace
        lacij.graph.core
        lacij.graph.svg.graph
        lacij.utils.core
        lacij.layouts.core
        lacij.layouts.layout))

(defn assoc-node-to-layer
  "Updates the layer-data structures and set the layer of a 
   given node."
  [layers-data layer node]
  (let [nodes (get (:layers layers-data) layer)
        nodes (conj nodes node)
        layers (assoc (:layers layers-data) layer nodes)]
    (assoc layers-data :layers layers)))

(defn add-to-tree
  "Adds children of nid to the tree."
  [tree nid children visited layer layers-data]
  (reduce (fn [[tree visited layers-data] child]
            (if (contains? visited child)
              [tree visited layers-data]
              (let [tree (add-node tree child (str (name child)))
                    eid (keyword (str (name nid) (name child)))
                    layers-data (assoc-in layers-data [:nodes child :layer] layer)
                    layers-data (assoc-node-to-layer layers-data layer child)]
                [(add-edge tree eid child nid) (conj visited child) layers-data])))
          [tree visited layers-data]
          children))

(defn explore-border
  "Adds nid to the tree and its children and extends the current border."
  [nid graph nextborder visited layer tree layers-data]
  (let [children (in-children graph nid)
        [tree visited layers-data]
        (add-to-tree tree nid children visited layer layers-data)]
    [(concat nextborder children) visited tree layers-data]))

(defn build-tree-helper
  "Explores each node on the border and extends the border
   until all nodes have been reached. At the end of the process, we have
   a tree from the graph."
  [graph border visited layer tree layers-data]
  (if (seq border)
    (let [[nextborder visited tree layers-data]
          (reduce (fn [[nextborder visited tree layers-data] nid]
                    (explore-border nid graph nextborder
                                    visited layer tree layers-data))
                  [() visited tree layers-data]
                  border)]
      (recur graph nextborder visited (inc layer) tree layers-data))
    [tree layers-data]))

(defn build-tree
  "Builds a tree from the graph and returns the layering information."
  [graph]
  (let [rootnode (find-root graph)
        tree (create-graph)
        tree (add-node tree rootnode (str (name rootnode)))
        layers-data {:layers [(list rootnode)] :nodes {rootnode {:layer 0}}}
        [tree layers-data] (build-tree-helper graph
                                              [rootnode]
                                              #{rootnode}
                                              1
                                              tree
                                              layers-data)]
    [tree layers-data]))

(defn leafs
  "Returns the leafs of a node in the tree."
  [tree nid]
  (leafs-seq #(seq (in-children tree %)) #(in-children tree %) nid))

(defn label-sizes
  "Calculates the sizes of all nodes in the tree. The size depends
   only of the number of leafs nodes."
  [tree layers-data]
  ;; this function could be optimized
  (reduce (fn [layers-data nid]
            (assoc-in layers-data [:sizes nid] (count (leafs tree nid))))
          layers-data
          (nodes tree)))

(defn label-angles-helper
  "Helper function to calculate the relative angle of each node in the tree."
  [tree layers-data nextchildren]
  (if-let [[[parentangle layer children] & res] nextchildren]
    (let [sizes (select-keys (:sizes layers-data) children)
          sum (apply + (vals sizes))
          [layers-data res]
          (reduce (fn [[layers-data res] child]
                    (let [size (get-in layers-data [:sizes child])
                          angle (* parentangle (/ size sum))
                          layers-data
                          (assoc-in layers-data [:angle child] angle)]
                      [layers-data (conj res [angle (inc layer)
                                              (in-children tree child)])]))
                  [layers-data res]
                  children)]
      (recur tree layers-data res))
    layers-data))

(defn label-angles
  "Calculates the relative angle of each node in the tree."
  [tree layers-data]
  (let [rootnode (ffirst (:layers layers-data))
        layers-data (assoc-in layers-data [:angle rootnode] 360)
        children (in-children tree rootnode)]
    (label-angles-helper tree layers-data [[360 2 children]])))

(defn angle [layers-data nid]
  "Returns the relative angle of a node."
  (get-in layers-data [:angle nid]))

(defn assoc-children-data
  "Calculates variious information for each node, such as the absolute angles
   and assoc them to the layers-data map."
  [layers-data children offset layer center-x center-y]
  (first
   (reduce (fn [[childrendata sum] child]
             (let [ang (angle layers-data child)
                   halfang (double (/ ang 2))
                   absolute-angle (+ sum halfang offset)
                   prevsum sum
                   sum (+ sum ang)]
               ;; (printf "child = %s offset = %s prevsum = %s sum = %s absolute = %s\n"
               ;;         child offset prevsum sum absolute-angle)
               [(conj childrendata {:child child
                                    :offset (+ offset prevsum)
                                    :absolute-angle
                                    absolute-angle
                                    :layer layer :center-x center-x :center-y center-y})
                sum]))
           [[] 0]
           children)))

(defn place-nodes-helper
  "Helper for the place nodes function."
  [graph tree layers-data radius nextchildren sort-children]
  (if (seq nextchildren) 
    (let [[child & res] nextchildren]
      (let [{:keys [child absolute-angle offset layer center-x center-y]} child
            ang (angle layers-data child)
            angle-radian (Math/toRadians absolute-angle)
            r (* radius layer)
            x (* r (Math/sin angle-radian))
            y (* r (Math/cos angle-radian))
            x (+ x center-x)
            y (- center-y y)
            graph (move-node graph child x y)
            children-of-child (sort-children (in-children tree child))
            reschildren (concat res (assoc-children-data
                                     layers-data
                                     children-of-child
                                     offset
                                     (inc layer)
                                     center-x
                                     center-y))]
        ;; (printf "  child = %s absolute = %s layer = %s  x = %s y = %s\n"
        ;;         child absolute-angle layer x y)
        (recur graph tree layers-data radius reschildren sort-children)))
    graph))

(defn place-nodes
  "Given layers-data and a tree, assign x-y coordinates to the nodes
   of the graph to build a hierarchical layout. "
  [graph tree layers-data width height radius sort-children] 
  (let [rootnode (ffirst (:layers layers-data))
        center-x (double (/ width 2))
        center-y (double (/ height 2))
        graph (move-node graph rootnode center-x center-y)
        children (assoc-children-data
                  layers-data (sort-children (in-children tree rootnode))
                  0 1 center-x center-y)]
    ;; (printf "root-node = %s center-x %s center-y %s\n" rootnode center-x center-y)
        (place-nodes-helper graph tree layers-data radius children sort-children)))

(defrecord RadialLayout
    []
  Layout

  (layout-graph
   [this graph options]
   ;; Layouts the graph radially.
   ;;
   ;;  Options are: width, height, radius.
   ;;
   (let [{:keys [width height radius sort-children]
            :or {width (width graph) height (height graph)
                 radius 180
                 sort-children identity}}
         options
         width (if (nil? width) 1900 width)
         height (if (nil? height) 1200 height)
         [tree layers-data] (build-tree graph)
         layers-data (label-sizes tree layers-data)
         layers-data (label-angles tree layers-data)
         graph (place-nodes graph tree layers-data width height radius
                            sort-children)]
     graph)
   ))

(defn radiallayout
  []
  (RadialLayout.))
