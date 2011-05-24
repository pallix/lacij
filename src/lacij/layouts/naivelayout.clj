;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "This layout uses a simulated annealing to
            minimize crossing and overlaping. Its energy function
            is really inefficient and it should be used only on small
            graphs.
            
            Based on an idea in the book 
            Programming Collective Intelligence, Toby Segaran, O'Reilly Media, 2007"}
  lacij.layouts.naivelayout
  (:use clojure.pprint
        lacij.utils.core
        lacij.opt.annealing
        (lacij.geom intersect distance)
        (lacij.layouts core randomlayout)
        lacij.graph.core
        lacij.view.core))

(defn x-edges
  "Returns the carthesian product of all edges, couples
   with two identical edges are removed."
  [graph]
  (let [alledges (edges graph)]
    (vec (for [eid1 alledges
               eid2 alledges
               :when (not= eid1 eid2)]
           [(edge graph eid1) (edge graph eid2)]))))

(defn x-nodesviews
  [graph]
  (let [allnodes (nodes graph)]
    (for [n1 allnodes
          n2 allnodes
          :when (not= n1 n2)]
      [(node-view (node graph n1)) (node-view (node graph n2))])))

(defn points [graph couple]
  "Returns the x1 y1 x2 y2 x3 y3 x4 y4 points of an edge couples"
  (mapcat #(edge-location (edge-view %) graph %) couple))

(defn count-crossing
  "Returns the number of crossing between edges"
  [graph couples]
  (reduce (fn [ncross couple]
            (let [pts (points graph couple)]
              (if (apply lines-intersect? pts)
                (inc ncross)
                ncross)))
          0
          couples))

(defn count-overlaps
  "Returns the number of overlaps between nodes"
  [graph nodesviews-couples]
  (reduce (fn [noverlaps couple]
            (if (apply rect-intersects? (mapcat bounding-box couple))
              (inc noverlaps)
              noverlaps))
          0
          nodesviews-couples))

(defn x-nodeviewedges
  [graph]
  (let [allnodes (nodes graph)
        alledges (edges graph)]
    (for [nid allnodes
          eid alledges
          :when (let [e (edge graph eid)
                      nsrc (src e)
                      ndst (dst e)]
                  (and (not= nsrc nid) (not= ndst nid)))]
      [(node-view (node graph nid))
       (edge graph eid)])))

(defn count-nodeedge-crossing
  [graph nodeviewedge-couples debug]
  (reduce (fn [ncross couple]
            (let [[view edge] couple
                  [x y width height] (bounding-box view)
                  [x1 y1 x2 y2] (edge-location (edge-view edge) graph edge)
                  intersect (line-intersects? x1 y1 x2 y2 x y width height)]
              (if intersect
                (inc ncross)
                ncross)))
          0
          nodeviewedge-couples))

(defn energy-ratio
  [value maxvalue]
  (if (= maxvalue 0)
    1
    (double (- 1 (/ value maxvalue)))))

(defn energy
  ([graph]
     (energy graph false))
  ([graph debug]
     (let [couples (x-edges graph)
           maxcrossing (count couples)
           nodesviewscouples (x-nodesviews graph)
           maxoverlaps (count nodesviewscouples)
           nodeviewedge-couples (x-nodeviewedges graph)
           maxnodecrossing (count nodeviewedge-couples)
           [ncrossing noverlaps nnodecrossing] [(count-crossing graph couples)
                                                (count-overlaps graph nodesviewscouples)
                                                (count-nodeedge-crossing graph nodeviewedge-couples false)]]
       (when debug
         (printf "maxcrossing = %s maxoverlaps = %s maxnodecrossing = %s\n"
                 maxcrossing maxoverlaps maxnodecrossing)
         (printf "ncrossing = %s, noverlaps = %s, nnodecrossing = %s\n"
                 ncrossing noverlaps nnodecrossing))
       (if (pos? noverlaps)
         0
         (let [e1 (energy-ratio ncrossing maxcrossing)
               e2 (energy-ratio nnodecrossing maxnodecrossing)
               e (+ (* 0.3 e1) (* 0.7 e2))]
           (when debug
             (printf "e1 = %s e2 = %s e = %s\n" e1 e2 e))
           e)))))

(defn- neighbour
  [width height graph]
  (let [id (rand-nth (nodes graph))]
    (move-node graph id (rand-int (- width 100)) (rand-int (- height 50)))))


(defrecord NaiveLayout
    []
  Layout

  (layout-graph
   [this graph options]
   (let [{:keys [width height]
            :or {width (width graph) height (height graph)}} options
            width (if (nil? width) 1024 width)
            height (if (nil? height) 768 height)
            graph (layout-graph (randomlayout) graph {:width width :height height})
         niter (* 25 (count (nodes graph)))
         graph (optimize graph energy (partial neighbour width height)
                         :iterations niter :init-temp 0.27 :calibration false)]
     ;; (printf "energy of the solution =\n")
     ;; (energy graph true)
     graph)))

(defn naivelayout
  []
  (NaiveLayout.))