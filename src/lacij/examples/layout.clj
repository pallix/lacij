(ns lacij.examples.layout
  (:use clojure.pprint
        lacij.edit.graph
        lacij.view.graphview
        (lacij.layouts core layout)))

(defn filter-opposite
  [s]
  (reduce
   (fn [s [a b]]
     (if (get s [b a])
       (disj s [a b])
       s))
   (set s) (set s)))

(defn gen-graph
  [nnodes nlinks]
  (let [genid (comp keyword gensym)
        geneid (partial (comp keyword gensym) "e")
        nodesid (take nnodes (repeatedly genid))
        edges (take nlinks
                    (shuffle
                     (filter-opposite
                      (for [src nodesid
                            dst nodesid
                            :when (not= src dst)]
                        [src dst]))))
        g (graph :width 800 :height 800)
        g (reduce (fn [g id]
                    (add-node g id (str id)))
                  g nodesid)
        g (reduce (fn [g [src dst]]
                    (let [eid (geneid)]
                     (add-edge g eid src dst)))
                  g
                  edges)]
    g))

(defn -main []
  (let [g (-> (gen-graph 12 12)
              (layout :naive)
              (build))]
    (export g "/tmp/layout.svg" :indent "yes")))
