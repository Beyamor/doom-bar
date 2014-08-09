(ns bar.ops
  (:require [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.system :refer [read-next-byte set-registers store-in-memory read-register
                                update-register set-flags read-registers read-register-address
                                read-memory return set-register read-next-word read-register-word
                                update-word-in-registers]]
            [bar.util :refer [truncate-byte bytes->word truncate-word word->bytes
                              word-half-carried? ->signed-byte int->bool bool->int in-range?]])
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
  [source h l]
  [2
   (m/do value    <- (read-register source)
         address  <- (read-register-word h l)
         (store-in-memory address value))])

(defn store-from-registers-address-and-update
  [h l update]
  [2
   (m/do value    <- (read-register :a)
         address  <- (read-register-word h l)
         (store-in-memory address value)
         (update-word-in-registers h l update))])

(defn store-from-registers-address-and-increment
  [h l]
  (store-from-registers-address-and-update h l inc))

(defn store-from-registers-address-and-decrement
  [h l]
  (store-from-registers-address-and-update h l dec))

(defn load-from-registers-address
  [r h l]
  [1
   (m/do value <- (read-register-address h l)
         (set-register r value))])

(defn load-from-registers-address-and-update
  [f]
  (fn [r h l]
    [1
     (m/do value <- (read-register-address h l)
           (set-register r value)
           (update-word-in-registers h l f))]))

(def load-from-registers-address-and-increment
  (load-from-registers-address-and-update inc))

(def load-from-registers-address-and-decrement
  (load-from-registers-address-and-update dec))

(defn update-registers-address
  [h l update]
  (m/do address  <- (read-register-word h l)
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

(defn update-register-word
  [h l update]
  (m/do value <- (read-register-word h l)
        :let [[h-value l-value] (-> value update truncate-word word->bytes)]
        (set-registers h h-value
                       l l-value)))

(defn increment-register-word
  [h l]
  [1
   (update-register-word h l inc)])

(defn decrement-register-word
  [h l]
  [1
   (update-register-word h l dec)])

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
         (set-register r value))])

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

(def add-registers
  (fn [r1 r2]
    [1
     (m/do r2-value <- (read-register r2)
           {:keys [carried?]} <- (update-register r1 + r2-value)
           (set-flags :operation  false
                      :carry      carried?))]))

(defn subtract-registers
  [r1 r2]
  [1
   (m/do r2-value <- (read-register r2)
         {:keys [carried?]} <- (update-register r1 - r2-value)
         (set-flags :operation  true
                    :carry      carried?))])

(def add-register-words
  (fn [hr2 lr2]
    [1
     (m/do value1 <- (read-register-word :h :l)
           value2 <- (read-register-word hr2 lr2)
           :let [result           (+ value1 value2)
                 truncate-result  (truncate-word result)
                 [h l]            (word->bytes truncate-result)]
           (set-registers :h h
                          :l l)
           (set-flags :operation  false
                      :carry      (> result 0xffff)
                      :half-carry (word-half-carried? value1 value2 truncate-result)))]))

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

(def daa-codes [[0 0 [0x0 0x9] 0 [0x0 0x9]]  [0x00 0]
                [0 0 [0x0 0x8] 0 [0xa 0xf]]  [0x06 0]
                [0 0 [0x0 0x9] 1 [0x0 0x3]]  [0x06 0]
                [0 0 [0xa 0xf] 0 [0x0 0x9]]  [0x60 1]
                [0 0 [0x9 0xf] 0 [0xa 0xf]]  [0x66 1]
                [0 0 [0xa 0xf] 1 [0x0 0x3]]  [0x66 1]
                [0 1 [0x0 0x2] 0 [0x0 0x9]]  [0x60 1]
                [0 1 [0x0 0x2] 0 [0xa 0xf]]  [0x66 1]
                [0 1 [0x0 0x3] 1 [0x0 0x3]]  [0x66 1]
                [1 0 [0x0 0x9] 0 [0x0 0x9]]  [0x00 0]
                [1 0 [0x0 0x8] 1 [0x6 0xf]]  [0xfa 0]
                [1 1 [0x7 0xf] 0 [0x0 0x9]]  [0xa0 1]])

(def daa-tests
  (->> daa-codes
       (partition 2)
       (map (fn [[[operation? carry? high-range half-carry? low-range]
                  [addend carried?]]]
              (fn [registers high-bits low-bits]
                (letfn [(get-flag [flag]
                          (-> registers (registers/flag-set? flag) bool->int))]
                  (when (and (= operation?  (get-flag :operation))
                             (= carry?      (get-flag :carry))
                             (= half-carry? (get-flag :operation))
                             (apply in-range? high-bits high-range)
                             (apply in-range? low-bits low-range))
                    [addend (int->bool carried?)])))))))

(def daa
  [1
   (m/do registers <- read-registers
         :let [a                  (registers :a)
               high-bits          (-> a (bit-shift-right 4) (bit-and 2r1111))
               low-bits           (-> a (bit-and 2r1111))
               [addend carried?]  (some #(% registers high-bits low-bits) daa-tests)]
         {:keys [zero?]} <- (update-register :a + addend)
         (set-flags :carry      carried?
                    :zero       zero?
                    :half-carry false))])

(def ones-complement
  [1
   (m/do (update-register :a bit-xor 0xff)
         (set-flags :half-carry true
                    :operation  true))])

(defn store-immediate-value-to-register-address 
  [h l]
  [2
   (m/do value    <- read-next-byte
         address  <- (read-register-word h l)
         (store-in-memory address value))])

(def set-carry-flag
  [1
   (set-flags :carry      true
              :half-carry false
              :operation  false)])

(def complement-carry-flag
  [1
   (m/do registers <- read-registers
         :let [carry? (registers/flag-set? registers :carry)]
         (set-flags :carry      (not carry?)
                    :half-carry false
                    :operation  false))])

(defn load-register-into-register
  [source dest]
  [1
   (m/do source-value <- (read-register source)
         (set-register dest source-value))])
