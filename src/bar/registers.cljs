(ns bar.registers
  (:require-macros [lonocloud.synthread :as ->]))

(def zeroed
  (into {}
        (for [register [:a :b :c :d :e :h :l
                        :pc :sp
                        :m :t]]
          [register 0])))

(def flags
  {:carry 0x10
   :zero 0x80
   :operator 0x40
   :half-carry 0x20})

(defn flag-set?
  [registers flag-name]
  (let [flag (get flags flag-name)]
    (-> registers :f (bit-and flag) (= flag))))

(defn set-flag
  [registers flag-name]
  (-> registers
      (update-in [:f] bit-or (get flags flag-name))))

(defn unset-flags
  [registers]
  (assoc registers :f 0))

(defn set-flags
  [registers & {:as conditions}]
  (reduce (fn [registers [flag-name set?]]
            (-> registers
                (->/when set?
                  (set-flag flag-name))))
          registers conditions))