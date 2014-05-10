(ns bar.ops
  (:require [bar.register :as register])
  (:require-macros [lonocloud.synthread :as ->])
  (:refer-clojure :exclude [apply]))

(def truncate (partial bit-and 0xff))

(defn half-carried?
  [addend1 addend2 result]
  (-> (bit-xor addend1 addend2 result)
      (bit-and 0x10)
      zero? not))

(def addr-e
  [1 4
   (fn [register]
     (let [result (+ (:a register) (:e register))
           truncated-result (truncate result)]
       (-> register
           (assoc :a truncated-result)
           (assoc :f 0)
           (register/set-flags
             :carry       (> result 255)
             :half-carry  (half-carried? (:a register) (:e register) truncated-result)
             :zero        (zero? truncated-result)))))])

(defn apply
  [register [m t f]]
  (-> register
      f
      (assoc :m m)
      (assoc :t t)))
