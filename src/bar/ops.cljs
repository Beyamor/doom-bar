(ns bar.ops
  (:require [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.system :refer [read-next-byte set-registers store-in-memory read-register
                                update-register set-flags read-registers read-register-address
                                read-memory return set-register read-next-word get-address-in-registers]]
            [bar.util :refer [truncate-byte bytes->word truncate-word word->bytes
                              word-half-carried? ->signed-byte]])
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
   (m/do value    <- (read-register :a)
         address  <- (get-address-in-registers h l)
         (store-in-memory address value))])

(defn load-from-registers-address
  [r h l]
  [1
   (m/do value <- (read-register-address h l)
         (set-register r value))])

(defn update-registers-address
  [h l update]
  (m/do address  <- (get-address-in-registers h l)
        memory   <- read-memory
        :let [value (-> memory (memory/load address) update truncate-byte)]
        (store-in-memory address value)))

(defn increment-registers-address 
  [h l]
  [1
   (update-registers-address h l inc)]) 

(defn decrement-registers-address 
  [h l]
  [1
   (update-registers-address h l dec)]) 

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
                                                   (-> shifted-a (->/when high? (bit-set 0)))))
         (set-flags :carry      carried?
                    :zero       false
                    :half-carry false
                    :operation  false))])

(def rla
  [1
   (m/do registers <- read-registers
         {:keys [carried?]} <- (update-register :a
                                                #(-> %
                                                     (bit-shift-left 1)
                                                     (->/when (registers/flag-set? registers :carry)
                                                       (bit-set 0))))
         (set-flags :carry      carried?
                    :zero       false
                    :half-carry false
                    :operation  false))])

(def rrca
  [1
   (m/do a <- (read-register :a)
         :let [low? (bit-test a 0)]
         (update-register :a
                          #(-> %
                               (bit-shift-right 1)
                               (->/when low?
                                 (bit-set 7))))
         (set-flags :carry      low?
                    :zero       false
                    :half-carry false
                    :operation  false))])

(def rra
  [1
   (m/do registers <- read-registers
         :let [low? (bit-test (registers :a) 0)]
         {:keys [carried?]} <- (update-register :a
                                                #(-> %
                                                     (bit-shift-right 1)
                                                     (->/when (registers/flag-set? registers :carry)
                                                       (bit-set 7))))
         (set-flags :carry      low?
                    :zero       false
                    :half-carry false
                    :operation  false))])

(def store-stack-pointer
  [3
   (m/do sp <- (read-register :sp)
         address <- read-next-word
         (store-in-memory address (-> sp truncate-byte))
         (store-in-memory (inc address) (-> sp (bit-shift-right 8) truncate-byte)))])

(def add-register-words
  (fn [hr2 lr2]
    [1
     (m/do h1 <- (read-register :h)
           l1 <- (read-register :l)
           h2 <- (read-register hr2)
           l2 <- (read-register lr2)
           :let [value1           (bytes->word h1 l1)
                 value2           (bytes->word h2 l2)
                 result           (+ value1 value2)
                 truncated-result (truncate-word result)
                 [h l]            (word->bytes truncated-result)]
           (set-registers :h h
                          :l l)
           (set-flags :operation  false
                      :carry      (> result 0xffff)
                      :half-carry (word-half-carried? value1 value2 truncated-result)))]))

(def immediate-relative-jump
  [2
   (m/do offset <- read-next-byte
         (update-register :pc + (->signed-byte offset)))])

(defn conditional-relative-jump
  [required-flags]
  [2
   (m/do offset <- read-next-byte
         registers <- read-registers
         :when (registers/flags-set? registers required-flags)
         (update-register :pc + (->signed-byte offset)))])
