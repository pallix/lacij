(ns lacij.examples.simple
  (:use lacij.edit.graph
        lacij.view.graphview))

(defn -main []
  (let [g (-> (graph :width 400 :height 400)
              (add-node :hermes "Hermes" :x 10 :y 30)
              (add-node :zeus "Zeus" :x 200 :y 125)
              (add-node :ares "Ares" :x 200 :y 225)
              (add-edge :father1 :hermes :zeus)
              (add-edge :father2 :ares :zeus)
              (build))]
    (export g "/tmp/simple.svg" :indent "yes")))