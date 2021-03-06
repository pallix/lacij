;;; Copyright © 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EPL V.1.0

(ns ^{:doc "Hit detections/intersections functions"}
  lacij.geom.intersect
  (:import java.awt.Rectangle
           java.awt.geom.Line2D
           java.awt.geom.Line2D$Double))

(defn line2d
  "Constructs a line2d object from the given points."
  [x1 y1 x2 y2]
  (Line2D$Double. x1 y1 x2 y2))

(defn lines-intersect?
  "Returns true if the line defined by (x1 y1, x2 y2) intersects
the line defined by (xx1, yy1, xx2, yy2)."
  [x1 y1 x2 y2 xx1 yy1 xx2 yy2]
  (.intersectsLine (line2d x1 y1 x2 y2) xx1 yy1 xx2 yy2))

(defn line-intersects?
  "Returns true if line (x1, y1) (x2 y2) 
   intersects rectangle (x3, y3, w, h)."
  [x1 y1 x2 y2 x3 y3 w h]
  (.intersects (line2d x1 y1 x2 y2) x3 y3 w h))

(defn rect-intersects?
  "Returns true if the rectangle defined by (x1, y1, w1, h1) intersects
with the rectangle defined by (x2, y2, w2, h2)."
  [x1 y1 w1 h1 x2 y2 w2 h2]
  (.intersects (Rectangle. x1 y1 w1 h1) (Rectangle. x2 y2 w2 h2)))