;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Simulating annealing optimization."}
  lacij.opt.annealing
  (:import java.util.Random))

(defn optimize
  "Returns the best state according to a
   simulated annealing optimization.
   
   Options: the probability p function
            the initial temperature init-temp
            the cooling rate cooling-rate
            the number of iterations iterations

   See: ﻿Busetti, Franco. 1983. \"Simulated annealing overview.\"
        http://en.wikipedia.org/wiki/Simulated_annealing"
  ([state energy neighbour & options]
     (let [options (apply hash-map options)
           cool-temp (fn [temp cooling-rate]
                       (* temp cooling-rate))
           pfunc (fn [eprev enew t]
                   (let [delta (- enew eprev)
                         p (Math/exp (/ delta t))]
                     ;; (printf "delta = %s\n" delta)
                     ;; (printf "t = %s\n" t)
                     ;; (printf "p = %s\n" p)
                     p))
           {:keys [p init-temp cooling-rate iterations calibration]
            :or {p pfunc
                 init-temp 250
                 cooling-rate 0.95
                 iterations 25
                 calibration false}} options
           e (energy state)
           random (Random.)]
       (let [sol
             (reduce (fn [{:keys [state e ebest sbest temp increases] :as conf} _]
                       (let [snew (neighbour state)
                             enew (energy snew)
                             temp (cool-temp temp cooling-rate)
                             conf (if (> enew ebest)
                                    (assoc conf :temp temp :ebest enew :sbest snew)
                                    (assoc conf :temp temp))]
                         ;; (printf "enew = %s\n" enew)
                         (cond (> enew e) 
                               (assoc conf :temp temp :state snew :e enew
                                  :increases (conj increases (- enew e)))

                               (< (.nextDouble random) (p e enew temp))
                               (assoc conf :temp temp :state snew :e enew)
                               
                               :else
                               conf)))
                     {:state state :e e :ebest e :sbest state :temp init-temp
                      :increases []}
                     (range iterations))
             increases (:increases sol)]
         (when calibration
           (let [avg (/ (apply + increases) (count increases))]
             (printf "Average increase = %s\n" avg)
             (printf "Suggested temperature = %s\n" (/ (- avg) (Math/log 0.8)))))
         (:sbest sol)))))
