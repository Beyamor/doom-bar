(ns bar.system
  (:require [bar.registers :as registers]
            [bar.memory :as memory])
  (:require-macros [bar.system.macros :as m])
  (:refer-clojure :exclude [get-in update-in]))

(def zeroed
  {:registers
   registers/zeroed

   :memory
   memory/zeroed})

(defn return
  [v]
  (fn [state]
    [v state]))

(defn bind
  [mv f]
  (fn [state]
    (let [[v state] (mv state)]
      ((f v) state))))

(defn run
  [state process]
  (-> state process second))

(defn get-in
  [path]
  (fn [system]
    [(clojure.core/get-in system path) system]))

(defn update-in
  [path f & args]
  (fn [system]
    (let [value (clojure.core/get-in system path)
          result (apply f value args)]
      [result (assoc-in system path result)])))

(def read-memory (get-in [:memory]))

(def read-registers (get-in [:registers]))

(defn read-register
  [register]
  (m/do registers <- read-registers
        (return (get registers register))))

(def read-next-byte
  (m/do memory <- read-memory
        pc <- (read-register :pc)
        (update-in [:registers :pc] inc)
        (-> memory (memory/load pc) return)))

(defn set-register
  [r value]
  (update-in [:registers] assoc r value))

(defn set-registers
  [& {:as registers}]
  (update-in [:registers] #(merge % registers)))

(defn store-in-memory
  [address value]
  (update-in [:memory] memory/store address value))

(defn read-register-address 
  [h l]
  (m/do registers <- read-registers
        (-> registers (registers/address h l) return)))

(defn update-register
  [r f & args]
  (m/do registers <- read-registers
        :let [result (apply registers/update-register registers r f args)]
        (set-register r (:value result))
        (return result)))

(defn set-flags
  [& flags]
  (update-in [:registers] #(apply registers/set-flags % flags)))

(defn set-flag
  [flag value]
  (apply set-flags flag value))
