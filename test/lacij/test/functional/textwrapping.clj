(ns lacij.test.functional.textwrapping
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph))

(defn -main []
  (let [g (-> (create-graph :width 800 :height 800)
              (add-node :wrap ["Wrap" "this" "text"] 10 30 :height 50)
              (add-node :circlewrap ["Wrap" "this" "text"]
                        60 130 :r 40 :shape :circle)
              (add-node :big "" 300 30 :height 350 :width 200)
              (add-label :big ["Wrap" "this" "big" "text"] :style {:font-size "50px"})
              (add-node :nowrap "Nowrap" 100 300)
              (build))]
    (export g "/tmp/textwrapping.svg" :indent "yes")))