(ns lacij.test.functional.styles
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph))

(defn -main []
  (-> (create-graph :width 800 :height 400)
      (add-default-node-style :fill "lightgreen")
      (add-default-edge-style :stroke "royalblue")
      (add-node :hermes "Hermes" :x 10 :y 30 :style {:fill "lightblue"})
      (add-node :zeus "Zeus" :x 300 :y 150 :rx 15 :ry 15)
      (add-node :ares "Ares" :x 300 :y 250 :style {:fill "lavender" :stroke "red"})
      (add-edge :father1 :hermes :zeus "son of"
                :style {:stroke "darkcyan" :stroke-dasharray "9, 5"})
      (add-edge :father2 :ares :zeus)
      (add-default-node-attrs :rx 5 :ry 5)
      (add-node :epaphus "Epaphus" :x 450 :y 250)
      (add-edge :epaphus-zeus :epaphus :zeus)
      (add-node :perseus "Perseus" :x 600 :y 150)
      (add-edge :perseus-zeus :perseus :zeus)
      (add-label :father2 "son of" :font-size "20px" :font-style "italic" :style {:stroke "crimson"})
      (build)
      (export "/tmp/styles.svg")))

