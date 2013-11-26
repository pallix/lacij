;;; Copyright © 2010-2011 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns lacij.view.utils.text
  (:use lacij.view.core
        lacij.utils.core
        lacij.view.utils.style)
  (:require [analemma.svg :as s]))

(defn view-labels
  [labels options]
  (let [{:keys [x y xmargin text-anchor text-anchor-multi x-multi y-multi]
         :or {x 0 y 0 xmargin 0 text-anchor "middle"
              x-multi 0 y-multi 0 text-anchor-multi "start"}} options]
    (map (fn [label]
           ;; TODO: use the position indicator
           (let [txt (:text label)
                 pos (:position label)
                 style (:style label)
                 font-size (or (:font-size (:attrs label)) 12)
                 font-family (or (:font-family (:attrs label)) "verdana")
                 ;; TODO support font size expressed in px
                 dy font-size
                 text (if (string? txt)
                        (s/text {:x x :y y
                                 :text-anchor text-anchor
                                 :font-size "12px"
                                 :font-family font-family}
                                txt)
                        (apply s/text {:text-anchor text-anchor-multi
                                       :font-size "12px"
                                       :font-family font-family
                                       :x x-multi
                                       :y y-multi}
                               (map (fn [s]
                                      (s/tspan {:dy dy :x xmargin} s))
                                    txt)))]
             (apply-styles text {:dominant-baseline :central} style)))
         labels)))
