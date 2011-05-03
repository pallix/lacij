;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Protocol for the undo/redo mechanism"}
  lacij.graph.svg.history)

(defprotocol History
  "A stack of states that can be undone or redone"
  (add-state [this state])
  (current-state [this])
  (update-current-state [this newstate])
  (can-undo-state? [this])
  (undo-state [this])
  (redo-state [this])
  (can-redo-state? [this]))

(defrecord SimpleHistory
    [states idx]
  History

  (add-state
   [this state]
   (let [stack this
         stack (update-in stack [:idx] inc)
         stack (update-in stack [:states] subvec 0 (:idx stack))
         stack (update-in stack [:states] conj state)]
     stack))

  (current-state
   [this]
   (get states idx))

  (update-current-state
   [this newstate]
   (assoc-in this [:states idx] newstate))

  (can-undo-state?
   [this]
   (not (neg? idx)))

  (can-redo-state?
   [this]
   (not= idx (dec (count states))))

  (undo-state
   [this]
   (if (can-undo-state? this)
     (let [currentstate (get states idx)]
       (update-in this [:idx] dec))
     this))

  (redo-state
   [this]
   (if (can-redo-state? this)
     (let [currentstate (get states idx)]
       (update-in this [:idx] inc))
     this)))

(defn simplehistory
  []
  (SimpleHistory. [] -1))
