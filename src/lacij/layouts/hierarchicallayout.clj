;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Hierarchical Layout.
           
            Implementation based on:

            How to draw a directed graph by Peter Eades, Kozo Sugiyama
            Journal of Information Processing (1990) Volume: 13, Issue: 4,
            Publisher: IEEE, Pages: 13-17"}
  lacij.layouts.hierarchicallayout
  (:use clojure.pprint
        lacij.geom.intersect
        lacij.layouts.core
        lacij.model.graph
        lacij.edit.graph
        lacij.view.core
        lacij.utils.core
        lacij.layouts.utils.position)
  (:require clojure.set))

(defn- get-layer
  [n layers]
  (get (:node-to-layer layers) n))

(defn- build-layer-to-node
  [layers]
  (reduce (fn [layers n]
            (let [layer (get-layer n layers)
                  nodes-at-layer (get-in layers [:layer-to-node layer] #{})
                  nodes-at-layer (conj nodes-at-layer n)]
              (assoc-in layers [:layer-to-node layer] (sort nodes-at-layer))))
          layers
          (keys (:node-to-layer layers))))

(defn- update-layers
  [graph n layers]
  (reduce (fn [{:keys [node-to-layer] :as layers} u]
            (let [layern (get node-to-layer n)
                  layeru (get node-to-layer u)
                  layer (Math/max layeru (inc layern))]
              (-> layers
                  (assoc-in [:node-to-layer u] layer))))
          layers
          (in-children graph n)))

(defn- longest-path-layering
  [context]
  (let [graph (:graph context)
        sorted (sort (vals (:nodes graph)))
        topology (topological-seq (constantly true) #(in-children graph %)
                                  (first sorted) sorted)
        layers (reduce (fn [layers n]
                         (update-layers graph n layers))
                       {:node-to-layer (apply hash-map (interleave topology (repeat 0)))
                        :layer-to-node (sorted-map)}
                       (reverse topology))]
    (assoc context :layers layers)))

(defn- span
  [n1 n2 layers]
  (let [l1 (get-layer n1 layers)
        l2 (get-layer n2 layers)]
    (- l2 l1)))

(defn- add-dummy-node
  [context nextnodes n1 n2]
  (let [{:keys [layers dummy-graph dummy-nodes segmented-edges]} context
        l1 (get-layer n1 layers)
        id (genid)
        dummygraph (add-node dummy-graph id (str id))
        dummygraph (remove-edge dummygraph n1 n2)
        e1 (geneid)
        dummygraph (add-edge dummygraph e1 n1 id)
        e2 (geneid)
        dummygraph (add-edge dummygraph e2 id n2)
        layers (assoc-in layers [:node-to-layer id] (dec l1))
        dummy-nodes (conj dummy-nodes id)]
    [(assoc context
       :dummy-graph dummygraph :layers layers :prev-node id
       :dummy-nodes dummy-nodes)
     (conj nextnodes id)]))

(defn- add-dummy-nodes-helper
  [context tovisit]
  (let [dummy-graph (:dummy-graph context)
        [n & nextnodes] tovisit]
    (if (nil? n)
      context
      (let [[context nextnodes] 
            (reduce (fn [[context nextnodes] u]
                      (let [{:keys [layers]} context
                            sp (span u n layers)]
                        ;; (printf "%s -> %s = %s\n" n u sp)
                        (if (> sp 1)
                          (add-dummy-node context nextnodes n u)
                          [context nextnodes])))
                    [context nextnodes]
                    (out-children dummy-graph n))]
        (recur context nextnodes)))))

(defn- add-dummy-nodes
  [context]
  (let [{:keys [graph layers]} context
        context (add-dummy-nodes-helper (assoc context :dummy-nodes #{})
                                        (vals (:nodes graph)))]
    (update-in context [:layers] build-layer-to-node)))

(defn- bary
  [u layeru context]
  (let [{:keys [layers dummy-graph]} context
        {:keys [layer-to-node]} layers
        nodes (get layer-to-node (dec layeru))
        vs (out-children dummy-graph u)
        val (/ (apply + (map #(index-of nodes %) vs)) (count vs))]
    ;; (printf "u = %s (%s) nodes = %s vs = %s => %s\n" u layeru nodes vs val)
    val
    ))

(defn- bary-sort
  [context layer]
  (let [layer-to-node (-> context :layers :layer-to-node)
        nodes (get layer-to-node layer)]
    (map first (sort-by second (map (fn [n] [n (bary n layer context)]) nodes)))))

(defn- bary-layer-by-layer
  [context]
  (reduce (fn [context layer]
            (assoc-in context [:layers :layer-to-node layer] (bary-sort context layer)))
          context
          (range 1 (count (-> context :layers :layer-to-node)))))

(defn- edge-weight
  [graph dummy-nodes eid]
  (let [e ((:edges graph) eid)
        s (:src e)
        d (:dst e)]
    (cond (and (dummy-nodes s) (dummy-nodes d))
          8

          (or (dummy-nodes s) (dummy-nodes d))
          2

          :else
          1)))

(defn- x-priority
  [graph dummy-nodes n f]
  (apply + (map #(edge-weight graph dummy-nodes %) (f ((:nodes graph) n)))))

(defn- up-priority
  [graph dummy-nodes n]
  (x-priority graph dummy-nodes n :outedges))

(defn- down-priority
  [graph dummy-nodes n]
  (x-priority graph dummy-nodes n :inedges))

(defn- sort-by-priority
  [prioritized-nodes]
  ;; stable sort
  (let [p (group-by second prioritized-nodes)]
    (reduce (fn [res idx]
              (concat res (sort (map first (get p idx)))))
            []
            (sort-by identity > (keys p)))))

(defn- assign-updown-priorities
  [context]
  (let [{:keys [dummy-graph dummy-nodes layers]} context
        layer-to-node (:layer-to-node layers)
        allnodes (vals (:nodes dummy-graph))
        upnodes (sort-by-priority (map (fn [n] [n (up-priority dummy-graph dummy-nodes n)]) allnodes))
        downnodes (sort-by-priority (map (fn [n] [n (down-priority dummy-graph dummy-nodes n)]) allnodes))]
    (assoc context
      :upprioritized-nodes upnodes
      :downprioritized-nodes downnodes)))

(defn- x-median
  [dummy-graph n x-children]
  (let [innodes (x-children dummy-graph n)
        ycoords (sort (map #(second (center (:view ((:nodes dummy-graph) %)))) innodes))
        [up down] (divide ycoords)]
    (cond (empty? up)
          (second (center (:view ((:nodes dummy-graph) n))))
          
          (= (count up) (count down))
          (int (/ (+ (last up) (first down)) 2))

          :else
          (last up))))

(defn- down-median
  [dummy-graph n]
  (x-median dummy-graph n in-children))

(defn- up-median
  [dummy-graph n]
  (x-median dummy-graph n out-children))

(defn- upperdowner-nodes
  [dummy-graph n nodes placed]
  (butlast
   (reduce (fn [[upper downer found] c]
             (cond found
                   [upper (conj downer c) true]

                   (= n c)
                   [upper downer true]

                   :else
                   [(conj upper c) downer false]))
           [[] [] false]
           nodes)))

(defn- placedupper-node
  [u placed]
  (first (filter #(contains? placed %) (reverse u))))

(defn- placeddowner-node
  [d placed]
     (first (filter #(contains? placed %) d)))

(defn- upper-y
  [dummy-graph upper-node inlayer-space]
  (if (nil? upper-node)
    0
    (let [view (:view ((:nodes dummy-graph) upper-node))
          y (:y view)
          h (:height view)]
      (+ y h inlayer-space))))

(defn- downer-y
  [dummy-graph down-node inlayer-space]
  (if (nil? down-node)
    (/ Integer/MAX_VALUE 2)
    (let [y (:y (:view ((:nodes dummy-graph) down-node)))]
      (- y inlayer-space))))

(defn- nodes-between
  [upper-node n nodes]
  (first
   (reduce (fn [[between foundupper foundn] c]
             (cond (= c upper-node)
                   [between true false]

                   (= c n)
                   [between true true]

                   foundn
                   [between true true]

                   (or foundupper (nil? upper-node))
                   [(conj between c) true false]

                   :else
                   [between false false]))
           [[] false false]
           nodes)))

(defn- calc-space-for-nodes
  [dummy-graph nodes inlayer-space]
  (reduce (fn [space n]
            (let [h (:height (:view ((:nodes dummy-graph) n)))]
              (+ space h inlayer-space)))
          0
          nodes))

(defn- calc-upy
  [dummy-graph n nodes placed upperdowner inlayer-space]
  (let [[u d] upperdowner
        upper-node (placedupper-node u placed)
        upy (upper-y dummy-graph upper-node inlayer-space)
        nodesbetween (nodes-between upper-node n nodes)
        space (calc-space-for-nodes dummy-graph nodesbetween inlayer-space)
        y (+ upy space)]
    y))

(defn- calc-downy
  [dummy-graph n nodes placed upperdowner inlayer-space]
  (let [[_ d] upperdowner
        downernode (placeddowner-node d placed)
        downy (downer-y dummy-graph downernode inlayer-space)
        nodesbetween (nodes-between n downernode nodes)
        space (calc-space-for-nodes dummy-graph nodesbetween inlayer-space)
        y (- downy space)]
    y))

(defn- pack-node
  [n dummy-graph upy downy med]
  (let [view (:view ((:nodes dummy-graph) n))
        xcenter (first (center view))
        h (:height view)
        upmargin (int (- (- med (/ h 2)) upy))
        downmargin (int (- downy (+ med (/ h 2))))]
    (cond (neg? upmargin)
          (move-node-center dummy-graph n xcenter (+ med (- upmargin)))

          (neg? downmargin)
          (move-node-center dummy-graph n xcenter (- med (- downmargin)))

          :else
          (move-node-center dummy-graph n xcenter med))))

(defn- move-node-inlayer
  [dummy-graph n nodesinlayer med placed inlayer-space]
  (let [upperdowner (upperdowner-nodes dummy-graph n nodesinlayer placed)
        upy (calc-upy dummy-graph n nodesinlayer placed upperdowner inlayer-space)
        downy (calc-downy dummy-graph n nodesinlayer placed upperdowner inlayer-space)]
    (if (nil? med)
      dummy-graph
      (pack-node n dummy-graph upy downy med))))

(defn- assign-node-coordinates
  [dummy-graph n nodesinlayer placed inlayer-space x-median]
  (let [med (x-median dummy-graph n)
        dummy-graph (move-node-inlayer dummy-graph n nodesinlayer med placed inlayer-space)
        placed (conj placed n)]
    [dummy-graph placed]))

(defn- upward-iter2
  [context]
  ;; (printf "upward-iter2\n")
  (let [{:keys [dummy-graph upprioritized-nodes layers]} context
        layer-to-node (:layer-to-node layers)
        node-to-layer (:node-to-layer layers)
        dummy-graph (first (reduce (fn [[dummy-graph placed] n]
                                     (let [layer (node-to-layer n)]
                                       (assign-node-coordinates
                                        dummy-graph
                                        n
                                        (layer-to-node layer)
                                        placed
                                        (:inlayer-space context)
                                        up-median)))
                                   [dummy-graph #{}]
                                   upprioritized-nodes))]
    (assoc context :dummy-graph dummy-graph)))

(defn- downward-iter2
  [context]
  (let [{:keys [dummy-graph downprioritized-nodes layers]} context
        layer-to-node (:layer-to-node layers)
        node-to-layer (:node-to-layer layers)
        dummy-graph (first (reduce (fn [[dummy-graph placed] n]
                                     (let [layer (node-to-layer n)]
                                       (assign-node-coordinates
                                        dummy-graph
                                        n
                                        (layer-to-node layer)
                                        placed
                                        (:inlayer-space context)
                                        down-median)))
                                   [dummy-graph #{}]
                                   downprioritized-nodes))]
    (assoc context :dummy-graph dummy-graph)))

(defn- place-node-inlayer
  [dummy-graph nodes x inlayer-space]
  (first
   (reduce (fn [[dummy-graph y] n]
             (let [h (int (/ (:height (:view ((:nodes dummy-graph) n))) 2))
                   y (+ y h)
                   dummy-graph (move-node-center dummy-graph n x y)]
               [dummy-graph (+ y h inlayer-space)]))
           [dummy-graph 0]
           nodes)))

(defn- assign-default-coordinates
  [context]
  (let [{:keys [dummy-graph layers inlayer-space layer-space]} context
        layer-to-node (:layer-to-node layers)
        [dummy-graph _] (reduce (fn [[dummy-graph x] layer]
                                  (let [nodes (layer-to-node layer)
                                        width (widest-value dummy-graph nodes)
                                        x (+ x (by-two width))
                                        dummy-graph (place-node-inlayer dummy-graph nodes x inlayer-space)]
                                    [dummy-graph (+ x (by-two width) layer-space)]))
                                [dummy-graph (- (by-two(widest-value dummy-graph (layer-to-node 0))))]
                                (range (count layer-to-node)))]
    (assoc context :dummy-graph dummy-graph)))

(defn- minnode-helper
  [dummy-graph placed queue inqueue node-to-layer layer-to-node inlayer-space step]
  (let [[x & xs] queue]
    (if (or (nil? x) (neg? step))
      dummy-graph
      (let [[_ current] (center (:view ((:nodes dummy-graph) x)))
            down (down-median dummy-graph x)
            up (up-median dummy-graph x)
            med (int (/ (+ down up) 2))]
        (if (not= (int current) med)
          (let [layer (node-to-layer x)
                nodesinlayer (layer-to-node layer)
                dummy-graph (move-node-inlayer dummy-graph x nodesinlayer med placed inlayer-space)
                children (concat (in-children dummy-graph x) (out-children dummy-graph x) )
                childrennotinqueue (clojure.set/difference (set children) inqueue)]
            (recur dummy-graph
                   (apply disj (conj placed x) children)
                   (concat xs childrennotinqueue)
                   (into (disj inqueue x) childrennotinqueue)
                   node-to-layer
                   layer-to-node
                   inlayer-space
                   (dec step)))
          (recur dummy-graph (conj placed x) xs (disj inqueue x) node-to-layer layer-to-node inlayer-space
                 (dec step)))))))

(defn- minnode
  [context]
  (let [layers (:layers context)
        node-to-layer (:node-to-layer layers)
        layer-to-node (:layer-to-node layers)
        dummy-graph (:dummy-graph context)
        inlayer-space (:inlayer-space context)]
    (assoc context :dummy-graph
           (minnode-helper dummy-graph #{} (vec (vals (:nodes dummy-graph)))
                           (set (vals (:nodes dummy-graph)))
                           node-to-layer layer-to-node inlayer-space 3000))))

(defn- assign-coordinates
  [context]
  ;; we use the heuristics from:
  ;;
  ;; Gansner, E.R. et al. 1993. “A technique for drawing directed graphs.”
  ;; IEEE Transactions on Software Engineering ;; 19(3): 214-230.
  ;; Available at: http://ieeexplore.ieee.org/lpdocs/epic03/wrapper.htm?arnumber=221135.
  (let [{:keys [layers dummy-graph]} context
        context (assign-default-coordinates context)
        context (assign-updown-priorities context)
        context (-> context
                     (downward-iter2)
                     (downward-iter2)
                     (downward-iter2)
                     (downward-iter2)
                     (upward-iter2)
                     (upward-iter2)
                     (downward-iter2)
                     (downward-iter2))
        ]
    (-> context
        (update-in [:dummy-graph] make-graph-visible)
        (update-in [:dummy-graph] adjust-size))))

(defn- place-nodes
  "Places nodes in the graph according to their positions in the dummy graph"
  [context]
  (let [{:keys [dummy-graph dummy-nodes graph]} context
        graph 
        (reduce (fn [graph nid]
                  (if ((:nodes graph) nid)
                    (let [view (:view ((:nodes dummy-graph) nid))
                          x (:x view)
                          y (:y view)]
                      (move-node graph nid x y))
                    graph))
                graph
                (vals (:nodes dummy-graph)))]
    (assoc context :graph graph)))

(defn- get-segmented-path
  [dummy-graph dummy-nodes nid nid2]
  (loop [current nid2
         path [nid nid2]]
    (let [nextid (first (out-children dummy-graph current))]
      (cond (nil? nextid)
            path

            (get dummy-nodes nextid)
            (recur nextid (conj path nextid))

            :else
            (conj path nextid)))))

(defn- find-segmented-paths
  [graph nid dummy-graph dummy-nodes]
  (let [dummychildren (filter #(not (nil? (get dummy-nodes %))) (out-children dummy-graph nid))]
    (map #(get-segmented-path dummy-graph dummy-nodes nid %) dummychildren)))

(defn- create-segment
  [graph dummy-graph segment]
  (let [begin (first segment)
        end (last segment)
        points (map #(center (:view ((:nodes dummy-graph) %))) (butlast (rest segment)))
        edgeid (first (filter #(= end (:dst ((:edges graph) %)))
                              (:outedges ((:nodes graph) begin))))
        eview (:view ((:edges graph) edgeid))
        {:keys [labels style attrs]} eview
        graph (remove-edge graph begin end)
        labels (:labels eview)
        graph (add-segmented-edge-kv graph edgeid begin end (merge {:label "" :style style} attrs) points)
        graph (reduce (fn [graph label]
                        (let [txt (:text label)
                              params (dissoc label :text)]
                          (add-label-kv graph edgeid txt params)))
                      graph
                      labels)]
    ;; (p (edge graph edgeid))
    ;; (p (edge-labels eview))
    graph))

(defn- segment-outgoing-edges
  [graph nid dummy-graph dummy-nodes]
  ;; segment outgoing edges if necessary
  (let [segments (find-segmented-paths graph nid dummy-graph dummy-nodes)
        graph (reduce (fn [graph segment]
                        (create-segment graph dummy-graph segment))
                      graph
                      segments)]
    graph))

(defn- add-segments
  [context]
  (let [{:keys [graph dummy-graph dummy-nodes]} context
        graph (reduce (fn [graph nid]
                        (segment-outgoing-edges graph nid dummy-graph dummy-nodes))
                      graph
                      (vals (:nodes graph)))]
    (assoc context :graph graph)))

(defn- cleanup
  [context]
  (let [context (place-nodes context)
        context (add-segments context)]
    context))

(defrecord HierarchicalLayout
    []
  Layout
  
  (layout-graph
    ;; Available options are :layer-space and :inlayer-space
   [this graph options]
    (let [steps {:layering longest-path-layering
                 :dummy-nodes add-dummy-nodes
                 :crossing-minimization bary-layer-by-layer
                 :bendsangles-reduction identity
                 :coordinates-assignment assign-coordinates
                 :cleanup cleanup}
          context (merge {:graph graph :dummy-graph graph :inlayer-space 20 :layer-space 150}
                         options)
          ;; calls each step of the layout:
          context (->> context
                       ((:layering steps))
                       ((:dummy-nodes steps))
                       ((:crossing-minimization steps))
                       ((:bendsangles-reduction steps))
                       ((:coordinates-assignment steps))
                       ((:cleanup steps)))]
      (-> (:graph context)
          (make-graph-visible)
          (adjust-size)))))

(defn hierarchicallayout []
  (HierarchicalLayout.))

