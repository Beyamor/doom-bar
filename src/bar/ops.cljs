(ns bar.ops
  (:require [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.system :refer [set-registers]])
 (:require-macros [lonocloud.synthread :as ->]))

(defn execute
  [system [m f]]
  (-> system
      f
      (set-registers :m m)))

(def truncate (partial bit-and 0xff))

(defn half-carried?
  [addend1 addend2 result]
  (-> (bit-xor addend1 addend2 result)
      (bit-and 0x10)
      zero? not))

(def unimplemented-op
  [0
   (fn [_]
     (throw (js/Error. "Unimplemented op")))])

(def no-op
  [1 identity])

(defn addr
  [r]
  [1
   (fn [{:keys [registers] :as system}]
     (let [result (+ (registers :a) (registers r))
           truncated-result (truncate result)]
       (-> system
           (set-registers :a truncated-result
                          :f 0)
           (->/in [:registers]
                  (registers/set-flags
                    :carry       (> result 255)
                    :half-carry  (half-carried? (registers :a) (registers r) truncated-result)
                    :zero        (zero? truncated-result))))))])

(defn load-to-registers
  [r2 r1]
 [3
 (fn [{:keys [registers memory] :as system}]
  (-> system
      (set-registers r1 (->> registers :pc (memory/load memory))
                     r2 (->> registers :pc inc (memory/load memory)))
      (update-in [:registers :pc] + 2)))])


(defn store-from-registers-address
  [h l]
  [2
   (fn [{:keys [registers] :as system}]
     (let [address (bit-or
                     (-> registers h (bit-shift-left 8))
                     (-> registers l))
           value (:a registers)]
     (update-in system [:memory] memory/store address value)))])
