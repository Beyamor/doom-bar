(ns bar.util)

(defn in-range?
  [x lower upper]
  (and (>= x lower)
       (<= x upper)))
