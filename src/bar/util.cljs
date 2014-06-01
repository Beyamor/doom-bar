(ns bar.util)

(defn in-range?
  [x lower upper]
  (and (>= x lower)
       (<= x upper)))

(def truncate-byte (partial bit-and 0xff))

(def truncate-word (partial bit-and 0xffff))

(defn byte-half-carried?
  [addend1 addend2 result]
  (-> (bit-xor addend1 addend2 result)
      (bit-test 4)))

(defn word-half-carried?
  [addend1 addend2 result]
  (-> (bit-xor addend1 addend2 result)
      (bit-test 10)))

(defn bytes->word
  [high-byte low-byte]
  (bit-or (bit-shift-left high-byte 8)
          low-byte))

(defn word->bytes
  [word]
  [(-> word (bit-shift-right 8) truncate-byte)
   (-> word truncate-byte)])

(defn bool->int
  [true?]
  (if true? 1 0))

(defn ->signed-byte
  [byte]
  (+ (-> byte (bit-test 7) bool->int (* -128))
     (->> (range 7)
          (map (fn [bit]
                 (* (bool->int (bit-test byte bit))
                    (Math/pow 2 bit))))
          (reduce +))))
