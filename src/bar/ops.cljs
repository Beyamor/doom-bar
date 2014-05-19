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
     (let [address (registers/address registers h l)
           value (:a registers)]
     (update-in system [:memory] memory/store address value)))])

(defn increment-registers-address 
  [h l]
  [1
   (fn [{:keys [registers memory] :as system}]
     (let [address (registers/address registers h l)
           value (-> memory (memory/load address) inc truncate)]
       (update-in system [:memory] memory/store address value)))])

(defn increment-register
  [r]
  [1
   (fn [system]
     (let [registers (:registers system)
           value (-> registers r inc)
           truncated-value (truncate value)]
       (-> system
           (->/in [:registers]
                  (assoc r truncated-value)
                  (registers/set-flags
                    :zero (zero? truncated-value)
                    :half-carry (half-carried? (registers r) 1 truncated-value)
                    :operation false)))))])

(defn decrement-register
  [r]
  [1
   (fn [system]
     (let [registers        (:registers system)
           value            (-> registers r dec)
           truncated-value  (truncate value)]
       (-> system
           (->/in [:registers]
                  (assoc r truncated-value)
                  (registers/set-flags
                    :zero (zero? truncated-value)
                    :half-carry (half-carried? (registers r) 1 truncated-value)
                    :operation true)))))])

(defn load-immediate-value 
  [register]
  [2
   (fn [{:keys [memory registers] :as system}]
     (let [value  (memory/load memory
                               (registers :pc))]
       (-> system
           (->/in [:registers]
                  (assoc register value)
                  (update-in [:pc] inc)))))])

(def rlca
  [1
   (fn [system]
     (let [shifted-a  (-> system :registers :a (bit-shift-left 1))
           high?      (bit-test shifted-a 9)
           cycled-a   (-> shifted-a truncate (->/when high? (bit-or 1)))]
       (-> system
           (->/in [:registers]
                  (assoc :a cycled-a)
                  (registers/set-flags
                    :carry      high?
                    :zero       false
                    :half-carry false
                    :operation  false)))))])
