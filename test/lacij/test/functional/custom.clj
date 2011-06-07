(ns lacij.test.functional.custom 
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
        lacij.view.core
        analemma.xml
        (lacij.view.svg rectnodeview circlenodeview))
  (:require [analemma.svg :as svg]))

(defn create-node-view []
  (let [xmlcontent (parse-xml (slurp "test/lacij/test/functional/blue-rect.svg"))
        rect (import-rect xmlcontent :blueRectangle0 :blueRectangle0 0 0)]
    (fn [shape x y style attrs]
      (condp = shape
          :rect (assoc rect :x x :y y :id (genid))
          nil))))

(defrecord CrossDecorator
    []
  Decorator

  (decorate
    [this view context]
    (let [[centerx centery] (node-center view)
          width (node-width view)
          height (node-height view)]
      (-> (svg/path [:M [centerx (double (- centery (/ height 2)))]
                     :L [centerx (double (+ centery (/ height 2)))]
                     :M [(double (- centerx (/ width 2))) centery]
                     :L [(double (+ centerx (/ width 2))) centery]
                     :Z []])
          (svg/style :stroke-width 3 :stroke "black")))))

(defn -main []
  (let [xmlcircle (parse-xml (slurp "test/lacij/test/functional/green-circle.svg"))]
   (-> (create-graph)
       (set-node-view-factory (create-node-view))
       (add-node :athena "Athena" 10 30)
       (add-node :zeus "Zeus" 200 150)
       (add-node :hera "Hera" 500 150)
       (add-node :ares "Ares" 350 250)
       (add-node :cross-rect "" 450 350 :shape :rect)
       (add-decorator :cross-rect (CrossDecorator.))
       (add-node :cross-circle "" 550 450 :shape :circle)
       (add-decorator :cross-circle (CrossDecorator.))
       (add-node :matrimony "♥" 0 0 :shape :circle)
       (set-node-view :matrimony (import-circle xmlcircle :greenCircle0 :greenCircle0 400 170))
       (add-edge :father1 :athena :zeus)
       (add-edge :zeus-matrimony :zeus :matrimony)
       (add-edge :hera-matrimony :hera :matrimony)
       (add-edge :son-zeus-hera :ares :matrimony)
       (build)
       (export "/tmp/custom.svg" :indent "yes"))))

