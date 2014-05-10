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
           truncated-result (bit-and result 0xff)]
       (-> register
           (assoc :a truncated-result)
           (assoc :f 0)
           (->/when (> result 255)
             (register/set-flag :carry))
           (->/when (half-carried? (:a register) (:e register) truncated-result)
             (register/set-flag :half-carry))
           (->/when (zero? truncated-result)
             (register/set-flag :zero)))))])

(defn apply
  [register [m t f]]
  (-> register
      f
      (assoc :m m)
      (assoc :t t)))
