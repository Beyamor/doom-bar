(ns bar.ops
  (:require [bar.registers :as registers]
            [bar.system :refer [set-registers]])
 (:require-macros [lonocloud.synthread :as ->]))

(def truncate (partial bit-and 0xff))

(defn half-carried?
  [addend1 addend2 result]
  (-> (bit-xor addend1 addend2 result)
      (bit-and 0x10)
      zero? not))

(def addr-e
  [1 4
   (fn [{:keys [registers] :as system}]
     (let [result (+ (:a registers) (:e registers))
           truncated-result (truncate result)]
       (-> system
           (set-registers :a truncated-result
                          :f 0)
           (->/in [:registers]
                  (registers/set-flags
                    :carry       (> result 255)
                    :half-carry  (half-carried? (:a registers) (:e registers) truncated-result)
                    :zero        (zero? truncated-result))))))])

(defn execute
  [system [m t f]]
  (-> system
      f
      (set-registers :m m
                     :t t)))
