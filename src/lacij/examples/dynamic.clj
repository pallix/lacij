(ns lacij.examples.dynamic
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
        analemma.xml
        (tikkba swing dom core)
        tikkba.utils.xml
        (lacij.view.svg rectnodeview circlenodeview))
  (:import (javax.swing JFrame JButton BoxLayout SwingUtilities)
           (java.awt.event ActionListener)
           java.awt.Component))

;; copied from swing utils
(defn add-action-listener
  "Adds an ActionLister to component. When the action fires, f will be
invoked with the event as its first argument followed by args.
Returns the listener."
  [component f & args]
  (let [listener (proxy [ActionListener] []
                   (actionPerformed [event] (apply f event args)))]
    (.addActionListener component listener)
    listener))

(defn on-action [event svgcanvas g]
  (do-batik
   svgcanvas
   (-> g
       (add-node! :appolon "Appolon" :x 50 :y 350)
       (add-edge! :appolon-athena :appolon :athena))))

(defn gen-graph
  []
  (-> (create-graph)
      (add-node :athena "Athena" :x 10 :y 30)
      (add-node :zeus "Zeus" :x 200 :y 150)
      (add-node :hera "Hera" :x 500 :y 150)
      (add-node :ares "Ares" :x 350 :y 250)
      (add-node :matrimony "♥" :x 400 :y 170 :shape :circle)
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
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))

