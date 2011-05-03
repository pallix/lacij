(ns lacij.test.functional.dynamic
  (:use clojure.pprint
        clojure.contrib.swing-utils
        lacij.graph.core
        lacij.graph.svg.graph
        analemma.xml
        (tikkba swing dom core)
        tikkba.utils.xml
        (lacij.view.svg rectnodeview circlenodeview))
  (:import (javax.swing JFrame JButton BoxLayout)
           (java.awt.event ActionListener)
           java.awt.Component))

(defn on-action [event svgcanvas g]
  (do-batik
   svgcanvas
   (-> g
       (add-node! :appolon "Appolon" 50 350)
       (add-edge! :appolon-athena :appolon :athena))))

(defn gen-graph
  []
  (-> (create-graph)
      (add-node :athena "Athena" 10 30)
      (add-node :zeus "Zeus" 200 150)
      (add-node :hera "Hera" 500 150)
      (add-node :ares "Ares" 350 250)
      (add-node :matrimony "♥" 400 170 :shape :circle)
      (add-edge :father1 :athena :zeus)
      (add-edge :zeus-matrimony :zeus :matrimony)
      (add-edge :hera-matrimony :hera :matrimony)
      (add-edge :son-zeus-hera :ares :matrimony)
      (build)))

(defn create-frame
  [svgcanvas g]
  (let [frame (JFrame.)
        button (JButton. "Action")
        pane (.getContentPane frame)]
    (add-action-listener button on-action svgcanvas g)
    (.setLayout pane (BoxLayout. pane BoxLayout/Y_AXIS))
    (.setAlignmentX button Component/CENTER_ALIGNMENT)
    (.add pane button)
    (.add pane svgcanvas)
    (.setSize frame 800 600)
    (.setSize svgcanvas 800 600)
    frame))

(defn -main []
  (let [g (gen-graph)
        doc (view g)
        _ (export g "/tmp/dynamic1.svg")
        svgcanvas (canvas g)
        frame (create-frame svgcanvas g)]
    (do-swing-and-wait
     (.setVisible frame true))))

