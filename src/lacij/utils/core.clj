;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Various utilities functions"}
  lacij.utils.core)

(defn assign-defaults [m & kv]
  (let [default-values (apply hash-map kv)]
    (merge
     default-values
     (apply hash-map
            (flatten
             (map (fn [[k v :as pair]]
                    (if (nil? v)
                      [k (get default-values k)]
                      pair))
                  m))))))

(defn leafs-seq
  [branch? children root]
  (let [leafs (fn leafs [node]
                (lazy-seq
                 (if-not (branch? node)
                   [node]
                   (mapcat leafs (children node)))))]
    (leafs root)))


;; from the joy of clojure book:

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))


(defn readr [prompt exit-code]
  (let [input (clojure.main/repl-read prompt exit-code)]
    (if (= input ::tl) 
      exit-code
      input)))


(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))


(defmacro break []
  `(clojure.main/repl
    :prompt #(print "debug=> ")
    :read readr
    :eval (partial contextual-eval (local-context))))