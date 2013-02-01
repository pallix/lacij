(ns lacij.examples.swing
  (:use lacij.edit.graph
        analemma.xml
        (tikkba swing dom)
        tikkba.utils.xml)
  (:import (javax.swing JFrame SwingUtilities)))

(defn -main []
  (let [g (-> (graph)
              (add-node :athena "Athena" :x 10 :y 30)
              (add-node :zeus "Zeus" :x 200 :y 150)
              (add-node :hera "Hera" :x 500 :y 150)
              (add-node :ares "Ares" :x 350 :y 250)
              (add-node :matrimony "♥" :x 400 :y 170 :shape :circle)
              (add-edge :father1 :athena :zeus)
              (add-edge :zeus-matrimony :zeus :matrimony)
              (add-edge :hera-matrimony :hera :matrimony)
              (add-edge :son-zeus-hera :ares :matrimony)
              (build))
        _ (Thread/sleep 1000) ;; wait for rendering, TODO: attach a listener instead
        doc (:xmldoc g)
        canvas (jsvgcanvas)
        frame (JFrame.)]
    (set-document canvas doc)
    (.add (.getContentPane frame) canvas)
    (.setSize frame 800 600)
    (.setSize canvas 800 600)
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))

