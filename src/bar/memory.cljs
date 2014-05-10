(ns bar.memory)

(def zeroed
  (->> 0 (repeat (Math/pow 2 16)) vec))
