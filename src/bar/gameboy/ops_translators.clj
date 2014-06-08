(ns bar.gameboy.ops-translators
  (:require [clojure.core.match :refer [match]]))

(defn symbol->keywords
  [symbol]
  (->> symbol
       name
       .toCharArray
       (map #(-> % str .toLowerCase keyword))))

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

           [[true _] [false 'A]]
           (let [[r1 r2] (symbol->keywords arg1)]
             `(bar.ops/store-from-registers-address ~r1 ~r2))

           [[false _] [false 'd16]]
           (let [[r1 r2] (symbol->keywords arg1)]
             `(bar.ops/load-to-registers ~r1 ~r2))

           [[false _] [false 'd8]]
           (let [[r] (symbol->keywords arg1)]
             `(bar.ops/load-immediate-value ~r))

           [[true 'a16] [false 'SP]]
           `bar.ops/store-stack-pointer

           [[false _] [true _]]
           (let [[r] (symbol->keywords arg1)
                 [h l] (symbol->keywords arg2)]
             `(bar.ops/load-from-registers-address ~r ~h ~l)))))

(defmacro INC
  [arg1]
  (let [registers (symbol->keywords arg1)]
    (match (count registers)
           2
           (let [[r1 r2] registers]
             `(bar.ops/increment-registers-address ~r1 ~r2))

           1
           (let [[r] registers]
             `(bar.ops/increment-register ~r)))))

(defmacro DEC
  [arg1]
  (let [registers (symbol->keywords arg1)]
    (match (count registers)
           2
           (let [[r1 r2] registers]
             `(bar.ops/decrement-registers-address ~r1 ~r2))

           1
           (let [[r] registers]
             `(bar.ops/decrement-register ~r)))))

(defmacro ADD
  [arg1 arg2]
  (let [[h l] (symbol->keywords arg2)]
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
                                    symbol->keywords
                                    (map short-register-name->long-register-name)
                                    vec)]
            `(bar.ops/conditional-relative-jump ~required-flags)))))
