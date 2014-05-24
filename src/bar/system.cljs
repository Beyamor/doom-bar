(ns bar.system
  (:require [bar.registers :as registers]
            [bar.memory :as memory]))

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
