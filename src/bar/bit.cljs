(ns bar.bit
  (:refer-clojure :exclude [set?]))

(defn set?
  [value index]
  (-> value (bit-shift-right index) (bit-and 1) (= 1)))
