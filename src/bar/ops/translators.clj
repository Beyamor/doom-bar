(ns bar.ops.translators)

(defmacro ld
  [arg1 arg2]
  (let [[r1 r2] (->> arg1 name .toCharArray (map str) (map keyword))]
    `(bar.ops/load-to-registers ~r1 ~r2)))
