(ns bar.gameboy.ops-translators
  (:require [clojure.core.match :refer [match]]))

(defn symbol->keywords
  [split]
  (fn [symbol]
    (->> symbol
         name
         .toLowerCase
         split
         (map #(-> % str keyword)))))

(def symbol->registers
  (symbol->keywords #(re-seq #"a|b|c|d|e|h|l|pc|sp" %)))

(def symbol->flags
  (symbol->keywords #(.toCharArray %)))

(def short-register-name->long-register-name
  {:z :zero
   :n :operation
   :h :half-carry
   :c :carry})

(defmacro LD
  [arg1 arg2]
  (let [arg1-is-list? (list? arg1)
        arg1          (if arg1-is-list? (first arg1) arg1)
        arg2-is-list? (list? arg2)
        arg2          (if arg2-is-list? (first arg2) arg2)]
    (match [[arg1-is-list? arg1] [arg2-is-list? arg2]]
           [[true 'HL+] [false 'A]]
           `(bar.ops/store-from-registers-address-and-increment :h :l)

           [[true 'HL-] [false 'A]]
           `(bar.ops/store-from-registers-address-and-decrement :h :l)

           [[false 'A] [true 'HL+]]
           `(bar.ops/load-from-registers-address-and-increment :a :h :l)

           [[false 'A] [true 'HL-]]
           `(bar.ops/load-from-registers-address-and-decrement :a :h :l)

           [[true _] [false 'A]]
           (let [[r1 r2] (symbol->registers arg1)]
             `(bar.ops/store-from-registers-address ~r1 ~r2))

           [[false _] [false 'd16]]
           (let [[r1 r2] (symbol->registers arg1)]
             `(bar.ops/load-to-registers ~r1 ~r2))

           [[false _] [false 'd8]]
           (let [[r] (symbol->registers arg1)]
             `(bar.ops/load-immediate-value ~r))

           [[true 'a16] [false 'SP]]
           `bar.ops/store-stack-pointer

           [[false _] [true _]]
           (let [[r] (symbol->registers arg1)
                 [h l] (symbol->registers arg2)]
             `(bar.ops/load-from-registers-address ~r ~h ~l))
           
           [[true 'HL] [false 'd8]]
           `(bar.ops/store-immediate-value-to-register-address :h :l)
           
           [[false dest] [false source]]
           (let [[dest] (symbol->registers dest)
                 [source] (symbol->registers source)]
             `(bar.ops/load-register-into-register ~source ~dest)))))

(defmacro INC
  [arg]
  (let [is-list?  (list? arg)
        registers (symbol->registers
                    (if is-list? (first arg) arg))]
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
  (let [is-list?  (list? arg)
        registers (symbol->registers
                    (if is-list? (first arg) arg))]
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
  (let [[h l] (symbol->registers arg2)]
    `(bar.ops/add-register-words ~h ~l)))

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
