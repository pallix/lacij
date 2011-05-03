(ns lacij.test.functional.def
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
        analemma.svg
        analemma.xml))

(def dotl-marker
  [:dotl
   [:marker {:refX 0
             :refY 0
             :orient "auto"
             :style "overflow:visible"}
    (-> (path [:M [-2.5,-1.0]
               :C [-2.5,1.76 -4.74,4.0 -7.5,4.0]
               :C [-10.26,4.0 -12.5,1.76 -12.5,-1.0]
               :C [-12.5,-3.76 -10.26,-6.0 -7.5,-6.0]
               :C [-4.74,-6.0 -2.5,-3.76 -2.5,-1.0]
               :Z []])
        (add-attrs :transform "scale (0.8) translate (7.4, 1)"))]])

(defn -main []
  (-> (create-graph)
      (add-def dotl-marker)
      (add-node :hermes "Hermes" 10 30)
      (add-node :zeus "Zeus" 250 150)
      (add-node :ares "Ares" 250 250)
      (add-edge :father1 :hermes :zeus :marker-end "url(#dotl)")
      (add-edge :father2 :ares :zeus)
      (build)
      (export "/tmp/def.svg")))
