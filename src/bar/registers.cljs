(ns bar.registers
  (:require [bar.util :refer [truncate-byte truncate-word byte-half-carried? bytes->word]])
  (:require-macros [lonocloud.synthread :as ->]))

(def word-register? #{:pc :sp})

(def zeroed
  (into {}
        (for [register [:a :b :c :d :e :h :l
                        :pc :sp
                        :m :t]]
          [register 0])))

(def flags
  {:carry 0x10
   :zero 0x80
   :operation 0x40
   :half-carry 0x20})

(defn flag-set?
  [registers flag-name]
  (let [flag (get flags flag-name)]
    (-> registers :f (bit-and flag) (= flag))))

(defn flags-set?
  [registers flag-names]
  (every? #(flag-set? registers %) flag-names))

(defn set-flag
  [registers flag-name]
  (-> registers
      (update-in [:f] bit-or (get flags flag-name))))

(defn unset-flag
  [registers flag-name]
  (-> registers
      (update-in [:f] bit-and-not (get flags flag-name))))

(defn set-flags
  [registers & {:as conditions}]
  (reduce (fn [registers [flag-name set?]]
            (-> registers
                (->/if set?
                  (set-flag flag-name)
                  (unset-flag flag-name))))
          registers conditions))

(defn address
  [registers h l]
  (bytes->word (registers h) (registers l)))

(defn update-register
  [registers r f & args]
  (let [value           (get registers r)
        updated-value   (apply f value args)
        truncated-value (-> updated-value
                            (->/if (word-register? r)
                              truncate-word
                              truncate-byte))]
    {:value         truncated-value
     :zero?         (zero? truncated-value)
     :carried?      (> updated-value 0xff)
     :half-carried? #(byte-half-carried? value % truncated-value)}))
