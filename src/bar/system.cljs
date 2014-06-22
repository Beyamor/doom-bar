(ns bar.system
  (:require [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.util :refer [bytes->word word->bytes truncate-word]])
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

(def read-next-word
  (m/do low <- read-next-byte
        high <- read-next-byte
        (return (bytes->word high low))))

(defn set-register
  [r value]
  (update-in [:registers] assoc r value))

(defn set-registers
  [& {:as registers}]
  (update-in [:registers] #(merge % registers)))

(defn store-in-memory
  [address value]
  (update-in [:memory] memory/store address value))

(defn read-register-word 
  [h l]
  (m/do h <- (read-register h)
        l <- (read-register l)
        (return (bytes->word h l))))

(defn read-memory-at
  [address]
  (m/do memory <- read-memory
        (return (memory/load memory address))))

(defn read-register-address
  [h l]
  (m/do address <- (read-register-word h l)
        (read-memory-at address)))

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

(defn update-word-in-registers 
  [h l update]
  (m/do high-value <- (read-register h)
        low-value  <- (read-register l)
        :let [[high-value low-value] (-> (bytes->word high-value low-value)
                                         update
                                         truncate-word
                                         word->bytes)]
        (set-registers h high-value
                       l low-value)))
