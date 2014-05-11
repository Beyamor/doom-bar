(ns bar.ops.translators
  (:require [clojure.core.match :refer [match]]))

(defn symbol->keywords
  [symbol]
  (->> symbol
       name
       .toCharArray
       (map str)
       (map #(.toLowerCase %))
       (map keyword)))

(defmacro LD
  [arg1 arg2]
  (let [is-list? (list? arg1)]
    (match [is-list? arg1 arg2]
           [true _ a]
           (let [[r1 r2] (symbol->keywords (first arg1))]
             `(bar.ops/store-from-registers-address ~r1 ~r2))

           [false _ d16]
           (let [[r1 r2] (symbol->keywords arg1)]
             `(bar.ops/load-to-registers ~r1 ~r2)))))
