(ns bar.memory
  (:require [bar.util :refer [in-range?]]))

(def zeroed
  (->> 0 (repeat (Math/pow 2 16)) vec))

(defn mapped-address
  [address]
  (cond
    (in-range? address 0xE000 0xFDFF)
    (- address 0x2000)

    :else
    address))

(defn store
  [memory address value]
  (assoc memory (mapped-address address) value))

(defn load
  [memory address]
  (get memory (mapped-address address)))
