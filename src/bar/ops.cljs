(ns bar.ops
  (:require [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.system :refer [read-next-byte set-registers store-in-memory read-register
                                update-register set-flags read-registers read-register-address
                                read-memory return set-register]]
            [bar.util :refer [truncate-byte]])
 (:require-macros [lonocloud.synthread :as ->]
                  [bar.system.macros :as m]))

(defn execute
  [system [m f]]
  (-> system
      f
      second
      (assoc-in [:registers :m] m)))

(def unimplemented-op
  [0
   (fn [_]
     (throw (js/Error. "Unimplemented op")))])

(def no-op
  [1 (return nil)])

(defn addr
  [r]
  [1
   (m/do value <- (read-register r)
         {:keys [half-carried? zero? carried?]} <- (update-register :a + value)
         (set-flags :carry      carried?
                    :half-carry (half-carried? value)
                    :zero       zero?))])

(defn load-to-registers
  [r1 r2]
  [3
   (m/do byte2 <- read-next-byte
         byte1 <- read-next-byte
         (set-registers r1 byte1
                        r2 byte2))])

(defn store-from-registers-address
  [h l]
  [2
   (m/do {:keys [a] :as registers} <- read-registers
         :let [address (registers/address registers h l)]
         (store-in-memory address a))])

(defn increment-registers-address 
  [h l]
  [1
   (m/do address  <- (read-register-address h l)
         memory   <- read-memory
         :let [value (-> memory (memory/load address) inc truncate-byte)]
         (store-in-memory address value))]) 

(defn increment-register
  [r]
  [1
   (m/do {:keys [zero? half-carried?]} <- (update-register r inc)
         (set-flags :zero       zero?
                    :half-carry (half-carried? 1)
                    :operation  false))])

(defn decrement-register
  [r]
  [1
   (m/do {:keys [zero? half-carried?]} <- (update-register r dec)
         (set-flags :zero       zero?
                    :half-carry (half-carried? 1)
                    :operation  true))])

(defn load-immediate-value 
  [r]
  [2
   (m/do value <- read-next-byte
         (set-registers r value))])

(def rlca
  [1
   (m/do {:keys [carried?]} <- (update-register :a
                                                #(let [shifted-a (bit-shift-left % 1)
                                                       high?     (bit-test shifted-a 8)]
                                                   (-> shifted-a (->/when high? (bit-or 1)))))
         (set-flags :carry      :carried?
                    :zero       false
                    :half-carry false
                    :operation  false))])
