(ns bar.gameboy.ops-translators
  (:require [clojure.core.match :refer [match]]))

(defn symbol->keywords
  [split]
  (fn [symbol]
    (->> symbol
         name
         .toLowerCase
         split
         (map #(-> % str keyword))
         vec)))

(def symbol->registers
  (symbol->keywords #(re-seq #"a|b|c|d|e|h|l|pc|sp" %)))

(def symbol->flags
  (symbol->keywords #(.toCharArray %)))

(defn symbol->args
  [arg]
  (if (#{'d8 'd16 'a16 'HL+ 'HL-} arg)
    [arg]
    (symbol->registers arg)))

(defn list?-and-args
  [arg]
  (let [is-list? (sequential? arg)]
    [is-list? (symbol->args
                (if is-list? (first arg) arg))]))

(def short-register-name->long-register-name
  {:z :zero
   :n :operation
   :h :half-carry
   :c :carry})

(defmacro LD
  [arg1 arg2]
  (let [[arg1-is-list? arg1] (list?-and-args arg1)
        [arg2-is-list? arg2] (list?-and-args arg2)]
    (match [[arg1-is-list? arg1] [arg2-is-list? arg2]]
           [[true ['HL+]] [false [:a]]]
           `(bar.ops/store-from-registers-address-and-increment :h :l)

           [[true ['HL-]] [false [:a]]]
           `(bar.ops/store-from-registers-address-and-decrement :h :l)

           [[false [:a]] [true ['HL+]]]
           `(bar.ops/load-from-registers-address-and-increment :a :h :l)

           [[false [:a]] [true ['HL-]]]
           `(bar.ops/load-from-registers-address-and-decrement :a :h :l)

           [[false [r1 r2]] [false ['d16]]]
           `(bar.ops/load-to-registers ~r1 ~r2)

           [[false [r]] [false ['d8]]]
           `(bar.ops/load-immediate-value ~r)

           [[true ['a16]] [false [:sp]]]
           `bar.ops/store-stack-pointer

           [[false [r]] [true [h l]]]
           `(bar.ops/load-from-registers-address ~r ~h ~l)

           [[true [:h :l]] [false ['d8]]]
           `(bar.ops/store-immediate-value-to-register-address :h :l)

           [[true [h l]] [false [source]]]
           `(bar.ops/store-from-registers-address ~source ~h ~l)

           [[false [dest]] [false [source]]]
           `(bar.ops/load-register-into-register ~source ~dest))))

(defmacro INC
  [arg]
  (let [[is-list? registers] (list?-and-args arg)]
    (match [is-list? (count registers)]
           [false 2]
           (let [[r1 r2] registers]
             `(bar.ops/increment-register-word ~r1 ~r2))

           [false 1]
           (let [[r] registers]
             `(bar.ops/increment-register ~r))

           [true 2]
           (let [[r1 r2] registers]
             `(bar.ops/increment-registers-address ~r1 ~r2)))))

(defmacro DEC
  [arg]
  (let [[is-list? registers] (list?-and-args arg)]
    (match [is-list? (count registers)]
           [false 2]
           (let [[r1 r2] registers]
             `(bar.ops/decrement-register-word ~r1 ~r2))

           [false 1]
           (let [[r] registers]
             `(bar.ops/decrement-register ~r))

           [true 2]
           (let [[r1 r2] registers]
             `(bar.ops/decrement-registers-address ~r1 ~r2)))))

(defmacro ADD
  [arg1 arg2]
  (let [[arg1-is-list? registers1] (list?-and-args arg1)
        [arg2-is-list? registers2] (list?-and-args arg2)]
    (match [[arg1-is-list? registers1] [arg2-is-list? registers2]]
           [[false [h1 l1]] [false [h2 l2]]]
           `(bar.ops/add-register-words ~h1 ~l1 ~h2 ~l2)

           [[false [h l]] [false [:sp]]]
           `(bar.ops/add-sp-to-register-word ~h ~l)

           [[false [a]] [false [b]]]
           `(bar.ops/add-registers ~a ~b)

           [[false [r]] [true [h l]]]
           `(bar.ops/add-from-registers-address ~r ~h ~l))))

(defmacro JR
  ([arg]
   (match arg
          'r8
          `bar.ops/immediate-relative-jump))

  ([arg1 arg2]
   (match [arg1 arg2]
          [_ 'r8]
          (let [required-flags (->> arg1
                                    symbol->flags
                                    (map short-register-name->long-register-name)
                                    vec)]
            `(bar.ops/conditional-relative-jump ~required-flags)))))
