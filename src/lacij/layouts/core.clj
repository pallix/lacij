;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Layout protocol definition"}
  lacij.layouts.core)

(defprotocol Layout
  (layout-graph [this graph options])
  (layout-graph! [this graph options]))
