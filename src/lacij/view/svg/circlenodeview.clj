;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Implementation of the NodeView protocol for a circle"}
  lacij.view.svg.circlenodeview
  (:use clojure.pprint
        lacij.view.core
        lacij.view.svg.utils.style
        tikkba.dom)
  (:require [analemma.svg :as s]
            [analemma.xml :as xml]
            [tikkba.utils.dom :as dom])
  (:import java.lang.Math))

(defrecord SvgCircleNodeView
    [id x y radius labels default-style style attrs]
  NodeView

  (node-center
   [this]
   [x y])

  (node-x
   [this]
   x)

  (node-y
   [this]
   y)

  (add-node-label
   [this label]
   (update-in this [:labels] conj label))

  (node-labels
   [this]
   labels)

  (view-node
   [this node context]
   (let [{:keys [doc]} context
         ;; TODO: use the position indicator
         texts (map (fn [label]
                      (let [txt (text label)
                            pos (position label)]
                        (-> (s/text {:x x :y y :text-anchor "middle"} txt)
                            (s/style :dominant-baseline :central))))
                    labels)
         xml (concat (s/group
                      (-> (s/circle x y radius)
                          (apply-styles default-style style)
                          (apply-attrs (merge attrs {:id (name id)}))))
                     texts)]
     (dom/elements doc *svg-ns* xml)))

  (ports
   [this]
   (map (fn [angle]
          (let [ra (Math/toRadians angle)]
            [(+ x (* radius (Math/cos ra)))
             (+ y (* radius (Math/sin ra)))]))
        (range 0 360 22)))

  (bounding-box
   [this]
   (let [margin 5]
    [(- x radius margin) (- y radius margin) (+ radius (* 2 margin)) (+ radius (* 2 margin))]))
  )

(defn import-circle
  [xmlcontent file-id id x y]
  (let [circle (first (xml/filter-xml xmlcontent [{:id (name file-id)}]))
        attrs (second circle)
        {:keys [r style]} attrs
        attrs (dissoc attrs :r :style :x :y :id :cx :cy)
        style (s/parse-inline-css style)
        r (Double/parseDouble r)]
    (SvgCircleNodeView. id x y r [] {} style attrs)))