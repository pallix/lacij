(ns lacij.examples.graphdeps
  (:use clojure.pprint
        lacij.edit.graph
        lacij.view.graphview
        (lacij.layouts core layout))
  (:import java.io.File))

;; taken from old contrib:
(defn clojure-source-file?
  "Returns true if file is a normal file with a .clj extension."
  [#^File file]
  (and (.isFile file)
       (.endsWith (.getName file) ".clj")))

;; taken from old contrib:
(defn find-clojure-sources-in-dir
  "Searches recursively under dir for Clojure source files (.clj).
  Returns a sequence of File objects, in breadth-first sort order."
  [#^File dir]
  ;; Use sort by absolute path to get breadth-first search.
  (sort-by #(.getAbsolutePath %)
           (filter clojure-source-file? (file-seq dir))))

(defn used-namespaces
  [nsform]
  ;; assume :use is in a list
  (let [useclause (first
                   (filter #(and (list? %) (= ':use (first %))) nsform))]
    (mapcat (fn [n]
           (if (or (list? n) (vector? n))
             (map #(str (first n) "." %) (rest n))
             [(str n)]))
         (rest useclause))))

(defn get-ns
  [nsform]
  (str (second nsform)))

(defn get-ns-form
  [filename]
  ;; assume ns is the first form
  (read-string (slurp filename)))

(defn dependency
  [deps filename]
  (let [nsform (read-string (slurp filename))]
   (assoc deps (get-ns nsform) (used-namespaces nsform))))

(defn dependency-graph
  [filenames]
  (reduce dependency {} filenames))

(defn add-ns-names
  [map deps]
  (reduce (fn [map nsname]
            (add-node map (keyword nsname) nsname :width 250))
          map
          (set (concat (keys deps) (apply concat (vals deps))))))

(defn link-ns-to-deps
  [map ns ns-which-used-it]
  (reduce (fn [map n]
            (printf "%s -> %s;\n" n ns)
            (add-edge map (geneid) (keyword ns) (keyword n)))
          map
          ns-which-used-it))

(defn add-ns-links
  [map deps]
  (reduce (fn [map nsname]
            (link-ns-to-deps map nsname (deps nsname)))
          map
          (keys deps)))

(defn build-svggraph
  [deps]
  (-> (graph)
      (add-ns-names deps)
      (add-ns-links deps)
      (layout :hierarchical)
      (build)))

(defn -main
  "Graphs the dependency of a project. Call it with the project directory as first argument."
  [& arg]
  (let [projectdir (first arg)
        srcfiles (find-clojure-sources-in-dir (File. (str projectdir "/src")))
        deps (dependency-graph srcfiles)
        map (build-svggraph deps)]
    (export map "/tmp/deps.svg")))
