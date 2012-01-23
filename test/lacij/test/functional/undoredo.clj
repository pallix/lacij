(ns lacij.test.functional.undoredo
  (:use clojure.pprint
        lacij.graph.core
        lacij.graph.svg.graph
        analemma.xml
        (tikkba swing dom core)
        tikkba.utils.xml
        lacij.graph.svg.history
        (lacij.view.svg rectnodeview circlenodeview))
  (:import org.apache.batik.apps.svgbrowser.DOMViewer
           (javax.swing SwingUtilities JFrame JButton BoxLayout JPanel)
           (java.awt.event ActionListener)
           (java.awt Component BorderLayout FlowLayout)))

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


(defn gen-graph
  []
  (-> (create-graph)
      (add-node :Heracles "Heracles" 10 30)
      (add-node :clickme "Click on me" 400 30)))

(def ^{:dynamic true} *graph* (atom (gen-graph)))

(defn random-color
  []
  (letfn [(color
           []
           (Integer/toHexString (rand-int 16)))]
    (apply format "#%s%s%s%s%s%s" (repeatedly 6 color))))

(defn on-do-listener
  [event]
  (let [g (deref *graph*)
        svgcanvas (canvas g)
        nodeid (keyword (gensym "appolon"))]
    (do-batik-update
      g
      (let [g (add-node-styles! g :Heracles :fill (random-color))
            g (add-node! g nodeid "Appolon" (rand-int 600) (rand-int 600))
            g (add-edge! g (keyword (gensym "edge")) nodeid :Heracles)
            g (set-node-selected! g nodeid true)]
        (reset! *graph* g)))))

(defn on-undo-listener
  [event]
  (let [g (deref *graph*)
        svgcanvas (canvas g)]
    (do-batik
     svgcanvas
     (when (can-undo? g)
       (let [g (undo! g)]
         (reset! *graph* g))))))

(defn on-redo-listener
  [event]
  (let [g (deref *graph*)
        svgcanvas (canvas g)]
    (do-batik
     svgcanvas
     (when (can-redo? g)
       (let [g (redo! g)]
         (reset! *graph* g))))))

(defn on-click-listener
  [event]
  (let [g (deref *graph*)
        svgcanvas (canvas g)]
    (do-batik-update
     g
     (let [g (add-node-styles! g :clickme :fill (random-color))]
       (reset! *graph* g)))))

(defn create-frame
  [svgcanvas]
  (let [actionspanel (JPanel. )
        frame (JFrame.)
        pane (.getContentPane frame)
        dobutton (JButton. "do")
        undobutton (JButton. "undo")
        redobutton (JButton. "redo")]
    (add-action-listener dobutton on-do-listener)
    (add-action-listener undobutton on-undo-listener)
    (add-action-listener redobutton on-redo-listener)
    (.setLayout actionspanel (FlowLayout.))
    (.add actionspanel dobutton)
    (.add actionspanel undobutton)
    (.add actionspanel redobutton)
    (.add pane actionspanel BorderLayout/PAGE_START)
    (.add pane svgcanvas BorderLayout/CENTER)
    (.setSize frame 800 600)
    (.setSize svgcanvas 800 600)
    frame))

(defn -main []
  (let [g (deref *graph*)
        doc (view g)
        svgcanvas (canvas g)
        updatemanager (update-manager svgcanvas)
        frame (create-frame svgcanvas)
        g (build g)
        g (add-listener g :clickme "click" on-click-listener)
        ]
    (reset! *graph* g)
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))




