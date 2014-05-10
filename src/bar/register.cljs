(ns bar.register
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
  [register flag-name]
  (let [flag (get flags flag-name)]
    (-> register :f (bit-and flag) (= flag))))

(defn set-flag
  [register flag-name]
  (-> register
      (update-in [:f] bit-or (get flags flag-name))))

(defn unset-flags
  [register]
  (assoc register :f 0))

(defn set-flags
  [register & {:as conditions}]
  (reduce (fn [register [flag-name set?]]
            (-> register
                (->/when set?
                  (set-flag flag-name))))
          register conditions))
