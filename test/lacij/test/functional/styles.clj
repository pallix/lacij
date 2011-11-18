(ns lacij.test.functional.styles
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph))

(defn -main []
  (-> (create-graph :width 800 :height 400)
      (add-default-node-style :fill "lightgreen")
      (add-default-edge-style :stroke "royalblue")
      (add-node :hermes "Hermes" 10 30 :style {:fill "lightblue"})
      (add-node :zeus "Zeus" 300 150 :rx 15 :ry 15)
      (add-node :ares "Ares" 300 250 :style {:fill "lavender" :stroke "red"})
      (add-edge :father1 :hermes :zeus "son of"
                :style {:stroke "darkcyan" :stroke-dasharray "9, 5"})
      (add-edge :father2 :ares :zeus)
      (add-default-node-attrs :rx 5 :ry 5)
      (add-node :epaphus "Epaphus" 450 250)
      (add-edge :epaphus-zeus :epaphus :zeus)
      (add-node :perseus "Perseus" 600 150)
      (add-edge :perseus-zeus :perseus :zeus)
      (add-label :father2 "son of" :font-size "20px" :font-style "italic" :style {:stroke "crimson"})
      (build)
      (export "/tmp/styles.svg")))

