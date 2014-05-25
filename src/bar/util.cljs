(ns bar.util)

(defn in-range?
  [x lower upper]
  (and (>= x lower)
       (<= x upper)))

(def truncate-byte (partial bit-and 0xff))

(defn half-carried?
  [addend1 addend2 result]
  (-> (bit-xor addend1 addend2 result)
      (bit-and 0x10)
      zero? not))

(defn bytes->word
  [high-byte low-byte]
  (bit-or (bit-shift-left high-byte 8)
          low-byte))
