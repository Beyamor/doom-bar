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
  (cond
    (list? arg1)
    (let [[r1 r2] (symbol->keywords (first arg1))]
      `(bar.ops/store-from-registers-address ~r1 ~r2))

    :else
    (let [[r1 r2] (symbol->keywords arg1)]
      `(bar.ops/load-to-registers ~r1 ~r2))))
