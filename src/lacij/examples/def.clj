(ns lacij.examples.def
  (:use lacij.edit.graph
        lacij.view.graphview
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
  (-> (graph)
      (add-def dotl-marker)
      (add-node :hermes "Hermes" :x 10 :y 30)
      (add-node :zeus "Zeus" :x 250 :y 150)
      (add-node :ares "Ares" :x 250 :y 250)
      (add-edge :father1 :hermes :zeus :marker-end "url(#dotl)")
      (add-edge :father2 :ares :zeus)
      (build)
      (export "/tmp/def.svg")))
