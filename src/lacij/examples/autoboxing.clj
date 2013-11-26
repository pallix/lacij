(ns lacij.examples.autoboxing
  (:use lacij.edit.graph
        lacij.edit.dynamic
        lacij.view.graphview)
  (require [tikkba.utils.dom :as dom])
  (:import org.apache.batik.swing.svg.SVGLoadEventDispatcherAdapter
           (javax.swing JFrame SwingUtilities)))

(defn autobox-and-export
  [graph]
  (let [g (autobox-texts! graph)]
    (export g "/tmp/autoboxing.svg" :indent "yes")))

(defn show-jframe
  [g]
  (let [frame (JFrame.)]
    (.add (.getContentPane frame) (:svgcanvas g))
    (.setSize frame 800 600)
    (.setSize (:svgcanvas g) 800 600)
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))

(defn -main []
  (let [g (-> (graph :width "500pt" :height "400pt")
            (add-node :node1 "This text is long but the rectangle containing it is automatically resized." :x 20 :y 30)
            (add-node :node2 ["This text is" "also long." "The rectangle containing the text" " is also resized automatically."]
                      :x 200 :y 125)
            (add-node :node3 "Stuff" :x 200 :y 225)
            (add-edge :e1 :node1 :node2)
            (add-edge :e2 :node2 :node3)
            (build :after autobox-and-export))]
    (show-jframe g)))
