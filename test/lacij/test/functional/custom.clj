(ns lacij.test.functional.custom
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
        analemma.xml
        (lacij.view.svg rectnodeview circlenodeview)))

(defn create-node-view []
  (let [xmlcontent (parse-xml (slurp "test/lacij/test/functional/blue-rect.svg"))
        rect (import-rect xmlcontent :blueRectangle0 :blueRectangle0 0 0)]
    (fn [shape x y style attrs]
      (condp = shape
          :rect (assoc rect :x x :y y)
          nil))))

(defn -main []
  (let [xmlcircle (parse-xml (slurp "test/lacij/test/functional/green-circle.svg"))]
   (-> (create-graph)
       (set-node-view-factory (create-node-view))
       (add-node :athena "Athena" 10 30)
       (add-node :zeus "Zeus" 200 150)
       (add-node :hera "Hera" 500 150)
       (add-node :ares "Ares" 350 250)
       (add-node :matrimony "♥" 0 0 :shape :circle)
       (set-node-view :matrimony (import-circle xmlcircle :greenCircle0 :greenCircle0 400 170))
       (add-edge :father1 :athena :zeus)
       (add-edge :zeus-matrimony :zeus :matrimony)
       (add-edge :hera-matrimony :hera :matrimony)
       (add-edge :son-zeus-hera :ares :matrimony)
       (build)
       (export "/tmp/custom.svg"))))

