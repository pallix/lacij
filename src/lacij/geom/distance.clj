;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Functions to calculate the distances between points."}
  lacij.geom.distance
  (:import (java.awt.geom Point2D Line2D)))

(defn distance
  ([x1 y1 x2 y2]
     (Point2D/distance x1 y1 x2 y2))
  ([x1 y1 x2 y2 px py]
     (Line2D/ptLineDist x1 y1 x2 y2 px py)))
