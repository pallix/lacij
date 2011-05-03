(ns lacij.test.functional.circle
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph))

(defn -main []
  (-> (create-graph)
      (add-node :athena "Athena" 10 30)
      (add-node :zeus "Zeus" 200 150)
      (add-node :hera "Hera" 500 150)
      (add-node :ares "Ares" 350 250)
      (add-node :matrimony "♥" 400 170 :shape :circle :style {:fill "salmon"})
      (add-edge :father1 :athena :zeus)
      (add-edge :zeus-matrimony :zeus :matrimony)
      (add-edge :hera-matrimony :hera :matrimony)
      (add-edge :son-zeus-hera :ares :matrimony)
      (build)
      (export "/tmp/circle.svg" :indent "yes")))

