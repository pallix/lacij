(ns lacij.test.functional.listener
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

(defn gen-graph
  []
  (-> (create-graph)
      (add-node :clickme "Click on me" :x 10 :y 30)
      (build)))

(defn create-frame
  [svgcanvas]
  (let [frame (JFrame.)
        pane (.getContentPane frame)]
    (.add pane svgcanvas)
    (.setSize frame 800 600)
    (.setSize svgcanvas 800 600)
    frame))

(defn random-color
  []
  (letfn [(color
           []
           (Integer/toHexString (rand-int 16)))]
    (apply format "#%s%s%s%s%s%s" (repeatedly 6 color))))

(def ^{:dynamic true} *graph* (atom nil))

(defn on-click-listener
  [event]
  (let [g (deref *graph*)
        svgcanvas (canvas g)
        nodeid (gensym "appolon")]
    (do-batik
     svgcanvas
     (reset! *graph*
             (-> g
                 (add-node! nodeid "Appolon" :x (rand-int 600) :y (rand-int 600))
                 (add-node-styles! :clickme :fill (random-color))
                 (add-edge! (gensym "appolon-clickme") nodeid :clickme))))))

(defn -main []
  (let [g (gen-graph)
        doc (view g)
        svgcanvas (canvas g)
        frame (create-frame svgcanvas)
        g (add-listener g :clickme "click" on-click-listener)]
    (reset! *graph* g)
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))
