(ns bar.system
  (:require [bar.registers :as registers]
            [bar.memory :as memory]))

(def zeroed
  {:registers
   registers/zeroed

   :memory
   memory/zeroed})

(defn set-register
  [system register value]
  (update-in system [:registers] assoc register value))

(defn set-registers
  [system & {:as register-values}]
  (reduce (fn [system [register value]]
            (set-register system register value))
          system register-values))
