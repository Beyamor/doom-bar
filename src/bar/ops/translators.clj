(ns bar.ops.translators)

(defn symbol->keywords
  [symbol]
  (->> symbol
       name
       .toCharArray
       (map str)
       (map keyword)))

(defmacro ld
  [arg1 arg2]
  (let [[r1 r2] (symbol->keywords arg1)]
    `(bar.ops/load-to-registers ~r1 ~r2)))
