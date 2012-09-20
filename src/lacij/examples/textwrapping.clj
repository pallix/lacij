(ns lacij.examples.textwrapping
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph))

(defn -main []
  (let [g (-> (create-graph :width 800 :height 800)
              (add-node :wrap ["Wrap" "this" "text"] :x 10 :y 30 :height 50)
              (add-node :circlewrap ["Wrap" "this" "text"]
                        :x 60 :y 130 :r 40 :shape :circle)
              (add-node :big "" :x 300 :y 30 :height 350 :width 200)
              (add-label :big ["Wrap" "this" "big" "text"] :font-size "50")
              (add-node :nowrap "Nowrap" :x 100 :y 300)
              (add-node :nowrapcircle "Nowrap" :x 230 :y 230 :shape :circle :r 40)
              (build))]
    (export g "/tmp/textwrapping.svg" :indent "yes")))