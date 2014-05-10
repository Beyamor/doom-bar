(ns bar.ops
  (:require [bar.registers :as registers])
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
   (fn [registers]
     (let [result (+ (:a registers) (:e registers))
           truncated-result (truncate result)]
       (-> registers
           (assoc :a truncated-result)
           (assoc :f 0)
           (registers/set-flags
             :carry       (> result 255)
             :half-carry  (half-carried? (:a registers) (:e registers) truncated-result)
             :zero        (zero? truncated-result)))))])

(defn apply
  [registers [m t f]]
  (-> registers
      f
      (assoc :m m)
      (assoc :t t)))
