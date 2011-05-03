(ns lacij.test.functional.simple
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph))

(defn -main []
  (let [g (-> (create-graph :width 400 :height 400)
              (add-node :hermes "Hermes" 10 30)
              (add-node :zeus "Zeus" 200 125)
              (add-node :ares "Ares" 200 225)
              (add-edge :father1 :hermes :zeus)
              (add-edge :father2 :ares :zeus)
              (build))]
    (export g "/tmp/simple.svg" :indent "yes")))
