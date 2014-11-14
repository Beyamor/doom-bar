(ns bar.ops-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not should-throw with]]
                   [lonocloud.synthread :as ->])
  (:require [speclj.core]
            [clojure.data :as data]
            [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.ops :as ops]
            [bar.system :as system]))

(describe "no-op"
          (with diff (second (data/diff
                               system/zeroed
                               (ops/execute system/zeroed ops/no-op))))
          (it "does nothing except increment the time"
              (should= @diff
                       {:registers {:m 1}})))

(describe "unimplemented-op"
          (it "should throw"
              (should-throw js/Error
                            (-> system/zeroed
                                (ops/execute ops/unimplemented-op)))))

(describe "a load to registers instruction"
          (with registers (-> system/zeroed
                              (->/in [:memory]
                                     (memory/store 0 2)
                                     (memory/store 1 3))
                              (ops/execute (ops/load-to-registers :b :c))
                              :registers))
          (it "should set the register values"
              (should= 2 (@registers :c))
              (should= 3 (@registers :b)))
          (it "should take three cycles"
              (should= 3 (@registers :m))))

(describe "a store from registers address instruction"
          (it "should store a value in memory"
              (should= 23 (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 23
                                            :b 9
                                            :c 8))
                              (ops/execute (ops/store-from-registers-address :a :b :c))
                              :memory
                              (memory/load 0x0908)))))

(describe "a store-from-registers-and-increment instruction"
          (with system (-> system/zeroed
                           (->/in [:registers]
                                  (assoc :a 0x56
                                         :b 0xff
                                         :c 0xff))
                           (ops/execute (ops/store-from-registers-address-and-increment :b :c))))
          (it "should store a value in memory"
              (should= 0x56 (-> @system :memory (memory/load 0xffff))))
          (it "should increment the registers"
              (should= 0 (-> @system :registers :b))
              (should= 0 (-> @system :registers :c))))

(describe "a store-from-registers-and-decrement instruction"
          (with system (-> system/zeroed
                           (->/in [:registers]
                                  (assoc :a 0x3c
                                         :h 0x8a
                                         :l 0x5c))
                           (ops/execute (ops/store-from-registers-address-and-decrement :h :l))))
          (it "should store a value in memory"
              (should= 0x3c (-> @system :memory (memory/load 0x8a5c))))
          (it "should decrement the registers"
              (should= 0x8a (-> @system :registers :h))
              (should= 0x5b (-> @system :registers :l))))

(describe "an increment registers address instruction"
          (it "should increment the contents of that memory location"
              (should= 2 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 9
                                           :c 8))
                             (->/in [:memory]
                                    (memory/store 0x0908 1))
                             (ops/execute (ops/increment-registers-address :b :c))
                             :memory
                             (memory/load 0x0908))))

          (it "should wrap"
              (should= 0 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 9
                                           :c 8))
                             (->/in [:memory]
                                    (memory/store 0x0908 0xff))
                             (ops/execute (ops/increment-registers-address :b :c))
                             :memory
                             (memory/load 0x0908)))))

(describe "an increment register instruction"
          (it "should increment the contents of that register"
              (should= 2 (-> system/zeroed
                             (assoc-in [:registers :b] 1)
                             (ops/execute (ops/increment-register :b))
                             :registers :b)))

          (it "should unset the operation flag"
              (should-not (-> system/zeroed
                              (->/in [:registers]
                                     (registers/set-flag :operation))
                              (ops/execute (ops/increment-register :b))
                              :registers
                              (registers/flag-set? :operation))))

          (with overflown-registers (-> system/zeroed
                                        (assoc-in [:registers :b] 0xff)
                                        (ops/execute (ops/increment-register :b))
                                        :registers))
          (it "should wrap"
              (should= 0 (@overflown-registers :b)))
          (it "should set the zero flag"
              (should (registers/flag-set? @overflown-registers :zero)))
          (it "should not set the carry flag"
              (should-not (registers/flag-set? @overflown-registers :carry)))

          (it "should set the half-carry flag"
              (should (-> system/zeroed
                          (assoc-in [:registers :b] 0xf)
                          (ops/execute (ops/increment-register :b))
                          :registers
                          (registers/flag-set? :half-carry)))))

(describe "an decrement register instruction"
          (it "should decrement the contents of that register"
              (should= 1 (-> system/zeroed
                             (assoc-in [:registers :b] 2)
                             (ops/execute (ops/decrement-register :b))
                             :registers :b)))

          (it "should set the operation flag"
              (should (-> system/zeroed
                          (ops/execute (ops/decrement-register :b))
                          :registers
                          (registers/flag-set? :operation))))

          (with underflown-registers (-> system/zeroed
                                         (assoc-in [:registers :b] 0)
                                         (ops/execute (ops/decrement-register :b))
                                         :registers))
          (it "should wrap"
              (should= 0xff (@underflown-registers :b)))
          (it "should not set the carry flag"
              (should-not (registers/flag-set? @underflown-registers :carry)))

          (it "should set the zero flag"
              (should (-> system/zeroed
                          (assoc-in [:registers :b] 1)
                          (ops/execute (ops/decrement-register :b))
                          :registers
                          (registers/flag-set? :zero))))

          (it "should set the half-carry flag"
              (should (-> system/zeroed
                          (assoc-in [:registers :b] 0x10)
                          (ops/execute (ops/decrement-register :b))
                          :registers
                          (registers/flag-set? :half-carry)))))

(describe "an decrement registers address instruction"
          (it "should decrement the contents of that memory location"
              (should= 1 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 9
                                           :c 8))
                             (->/in [:memory]
                                    (memory/store 0x0908 2))
                             (ops/execute (ops/decrement-registers-address :b :c))
                             :memory
                             (memory/load 0x0908))))

          (it "should wrap"
              (should= 0xff (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 9
                                           :c 8))
                             (->/in [:memory]
                                    (memory/store 0x0908 0))
                             (ops/execute (ops/decrement-registers-address :b :c))
                             :memory
                             (memory/load 0x0908)))))

(describe "a load immediate value to register instruction"
          (with registers (-> system/zeroed
                              (->/in [:memory]
                                     (memory/store 0 2))
                              (ops/execute (ops/load-immediate-value :b))
                              :registers))
          (it "should store the next value in the register"
              (should= 2 (@registers :b)))
          (it "should increment the program counter"
              (should= 0x001 (@registers :pc))))

(describe "the rlca instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x85)
                                     (registers/set-flags
                                       :carry       false
                                       :half-carry  true
                                       :zero        true
                                       :operation   true))
                              (ops/execute ops/rlca)
                              :registers))
          (it "should rotate the bits in A"
              (should= 0x0b (@registers :a)))
          (it "should set the carry flag"
              (should (registers/flag-set? @registers :carry)))
          (it "should unset the other flags"
              (should-not (registers/flag-set? @registers :half-carry))
              (should-not (registers/flag-set? @registers :zero))
              (should-not (registers/flag-set? @registers :operation))))

(describe "the rla instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x95)
                                     (registers/set-flags
                                       :carry       true
                                       :half-carry  true
                                       :zero        true
                                       :operation   true))
                              (ops/execute ops/rla)
                              :registers))
          (it "should rotate the bits in A"
              (should= 0x2b (@registers :a)))
          (it "should set the carry flag"
              (should (registers/flag-set? @registers :carry)))
          (it "should unset the other flags"
              (should-not (registers/flag-set? @registers :half-carry))
              (should-not (registers/flag-set? @registers :zero))
              (should-not (registers/flag-set? @registers :operation))))

(describe "the rrca instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x3b)
                                     (registers/set-flags
                                       :carry       false
                                       :half-carry  true
                                       :zero        true
                                       :operation   true))
                              (ops/execute ops/rrca)
                              :registers))
          (it "should rotate the bits in A"
              (should= 0x9d (@registers :a)))
          (it "should set the carry flag"
              (should (registers/flag-set? @registers :carry)))
          (it "should unset the other flags"
              (should-not (registers/flag-set? @registers :half-carry))
              (should-not (registers/flag-set? @registers :zero))
              (should-not (registers/flag-set? @registers :operation))))

(describe "the rra instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x81)
                                     (registers/set-flags
                                       :carry       false
                                       :half-carry  true
                                       :zero        true
                                       :operation   true))
                              (ops/execute ops/rra)
                              :registers))
          (it "should rotate the bits in A"
              (should= 0x40 (@registers :a)))
          (it "should set the carry flag"
              (should (registers/flag-set? @registers :carry)))
          (it "should unset the other flags"
              (should-not (registers/flag-set? @registers :half-carry))
              (should-not (registers/flag-set? @registers :zero))
              (should-not (registers/flag-set? @registers :operation))))

(describe "the store-stack-pointer instruction"
          (with memory (-> system/zeroed
                           (assoc-in [:registers :sp] 0xfff8)
                           (->/in [:memory]
                                  (memory/store 0 0x00)
                                  (memory/store 1 0xc1))
                           (ops/execute ops/store-stack-pointer)
                           :memory))
          (it "should store the stack pointer values"
              (should= 0xf8 (memory/load @memory 0xc100))
              (should= 0xff (memory/load @memory 0xc101))))

(describe "the add-register-words instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :h 0x8a
                                            :l 0x23
                                            :b 0x06
                                            :c 0x05)
                                     (registers/unset-flag :operation))
                              (ops/execute (ops/add-register-words :h :l :b :c))
                              :registers))
          (it "should add the values"
              (should= 0x90 (@registers :h))
              (should= 0x28 (@registers :l)))
          (it "should unset the operation flag"
              (should-not (registers/flag-set? @registers :operation)))
          (it "should set the half-carry flag"
              (should (registers/flag-set? @registers :half-carry)))

          (with registers2 (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :h 0x8a
                                            :l 0x23)
                                     (registers/unset-flag :operation))
                              (ops/execute (ops/add-register-words :h :l :h :l))
                              :registers))
          (it "should add the values"
              (should= 0x14 (@registers2 :h))
              (should= 0x46 (@registers2 :l)))
          (it "should set the half-carry flag"
              (should (registers/flag-set? @registers2 :half-carry)))
          (it "should set the carry flag"
              (should (registers/flag-set? @registers2 :carry))))

(describe "a load-from-registers-address"
          (it "should load a value from memory"
              (should= 0x2f (-> system/zeroed
                                (->/in [:memory]
                                       (memory/store 0x1234 0x2f))
                                (->/in [:registers]
                                       (assoc :b 0x12
                                              :c 0x34))
                                (ops/execute (ops/load-from-registers-address :a :b :c))
                                :registers :a))))

(describe "a load-from-registers-address-and-increment"
          (with system (-> system/zeroed
                           (->/in [:memory]
                                  (memory/store 0x01ff 0x56))
                           (->/in [:registers]
                                  (assoc :h 0x01
                                         :l 0xff))
                           (ops/execute (ops/load-from-registers-address-and-increment :a :h :l))))
          (it "should load a value from memory"
              (should= 0x56 (-> @system :registers :a)))
          (it "should increment the register values"
              (should= 0x02 (-> @system :registers :h))
              (should= 0x00 (-> @system :registers :l))))

(describe "a load-from-registers-address-and-decrement"
          (with system (-> system/zeroed
                           (->/in [:memory]
                                  (memory/store 0x8a5c 0x3c))
                           (->/in [:registers]
                                  (assoc :h 0x8a
                                         :l 0x5c))
                           (ops/execute (ops/load-from-registers-address-and-decrement :a :h :l))))
          (it "should load a value from memory"
              (should= 0x3c (-> @system :registers :a)))
          (it "should decrement the register values"
              (should= 0x8a (-> @system :registers :h))
              (should= 0x5b (-> @system :registers :l))))

(describe "the immediate-relative-jump instruction"
          (let [jump (fn [starting-address offset]
                       (-> system/zeroed
                           (->/in [:registers]
                                  (assoc :pc starting-address))
                           (->/in [:memory]
                                  (memory/store starting-address offset))
                           (ops/execute ops/immediate-relative-jump)
                           :registers :pc))]
            (it "should jump by the offset"
                (should= 201 (jump 150 50))
                (should= 101 (jump 150 -50))
                (should= (+ 0xff 50 1) (jump 0xff 50))
                (should= (+ 0xffff -50 2) (jump 0 -50)))))

(describe "the conditional-relative-jump instruction"
          (let [jump (fn [starting-address offset required-flags actual-flags]
                       (-> system/zeroed
                           (->/in [:registers]
                                  (assoc :pc starting-address)
                                  (#(reduce registers/set-flag % actual-flags)))
                           (->/in [:memory]
                                  (memory/store starting-address offset))
                           (ops/execute (ops/conditional-relative-jump required-flags))
                           :registers :pc))]
            (it "should jump by the offset when the flags are set"
                (should= 201 (jump 150 50 [:zero] [:zero]))
                (should= 101 (jump 150 -50 [:zero] [:zero]))
                (should= (+ 0xff 50 1) (jump 0xff 50 [:zero] [:zero]))
                (should= (+ 0xffff -50 2) (jump 0 -50 [:zero] [:zero])))

            (it "should not jump when the flags aren't set"
                (should= 151 (jump 150 50 [:zero] []))
                (should= 151 (jump 150 -50 [:zero] []))
                (should= 0x100 (jump 0xff 50 [:zero] []))
                (should= 1 (jump 0 -50 [:zero] [])))))

(describe "the daa instruction"
          (with system (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x45 :b 0x38)
                                     (registers/set-flag :half-carry)
                                     (registers/set-flag :zero))
                              (ops/execute (ops/add-registers :a :b))
                              (ops/execute ops/daa)))
          (it "should, I gunno, do its thing for additions"
              (should= 0x83 (-> @system :registers :a))
              (should-not (-> @system :registers (registers/flag-set? :carry)))
              (should-not (-> @system :registers (registers/flag-set? :zero))))
          (it "should unset half-carry"
              (should-not (-> @system :registers (registers/flag-set? :half-carry))))

          (with system2 (-> @system
                            (ops/execute (ops/subtract-registers :a :b))
                            (ops/execute ops/daa)))
          (it "should also do its thing for subtractions"
              (should= 0x45 (-> @system2 :registers :a))))

(describe "the ones-complement instruction"
          (with registers (-> system/zeroed
                                (->/in [:registers]
                                       (assoc :a 0x35))
                                (ops/execute ops/ones-complement)
                                :registers))

          (it "should set a to its one's complement"
              (should= 0xca (@registers :a)))

          (it "should set operation"
              (should (registers/flag-set? @registers :operation)))

          (it "should set half-carry"
              (should (registers/flag-set? @registers :half-carry))))

(describe "the increment-register-word instruction"
          (let [registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :d 0x23 :e 0x5f))
                              (ops/execute (ops/increment-register-word :d :e))
                              :registers)]
            (it "should increment the word in the registers"
                (should= 0x23 (registers :d))
                (should= 0x60 (registers :e)))))

(describe "the decrement-register-word instruction"
          (let [registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :d 0x23 :e 0x5f))
                              (ops/execute (ops/decrement-register-word :d :e))
                              :registers)]
            (it "should decrement the word in the registers"
                (should= 0x23 (registers :d))
                (should= 0x5e (registers :e)))))

(describe "the store-immediate-value-to-register-address instruction"
          (it "should store the immediate value in the address"
              (should= 0x12 (-> system/zeroed
                                (->/in [:registers]
                                       (assoc :h 0x8a :l 0xc5))
                                (->/in [:memory]
                                       (memory/store 0 0x12))
                                (ops/execute (ops/store-immediate-value-to-register-address :h :l))
                                :memory
                                (memory/load 0x8ac5)))))

(describe "the set-carry-flag instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (registers/set-flags :carry      false
                                                          :half-carry true
                                                          :operation  true
                                                          :zero       true))
                              (ops/execute ops/set-carry-flag)
                              :registers))

          (it "should set the carry flag"
              (should (registers/flag-set? @registers :carry)))

          (it "should unset the half-carry and operation flags"
              (should-not (registers/flag-set? @registers :half-carry))
              (should-not (registers/flag-set? @registers :operation)))

          (it "should leave the zero flag unchanged"
              (should (registers/flag-set? @registers :zero))))

(describe "the complement-carry-flag"
          (with carry-set-registers (-> system/zeroed
                                        (->/in [:registers]
                                               (registers/set-flags :carry      false
                                                                    :operation  true
                                                                    :half-carry true))
                                        (ops/execute ops/complement-carry-flag)
                                        :registers))

          (with carry-unset-registers (-> system/zeroed
                                          (->/in [:registers]
                                                 (registers/set-flags :carry      true
                                                                      :operation  true
                                                                      :half-carry true))
                                          (ops/execute ops/complement-carry-flag)
                                          :registers))

          (it "should set an unset flag"
              (should (registers/flag-set? @carry-set-registers :carry)))

          (it "should unset a set flag"
              (should-not (registers/flag-set? @carry-unset-registers :carry)))

          (it "should unset the half-carry and operation flags"
              (should-not (registers/flag-set? @carry-set-registers :half-carry))
              (should-not (registers/flag-set? @carry-set-registers :operation))
              (should-not (registers/flag-set? @carry-unset-registers :half-carry))
              (should-not (registers/flag-set? @carry-unset-registers :operation))))

(describe "the load-register-into-register instruction"
          (it "should load the contents of r' into r"
              (should= 0xb (-> system/zeroed
                               (->/in [:registers]
                                      (assoc :a 0xa :b 0xb))
                               (ops/execute (ops/load-register-into-register :b :a))
                               :registers
                               :a))))

(describe "the add-registers instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x3a :b 0xc6)
                                     (registers/set-flag :operation))
                              (ops/execute (ops/add-registers :a :b))
                              :registers))
          (it "should add the values and store them in the first register"
              (should= 0 (@registers :a)))

          (it "should set the zero, carry, and half-carry flags"
              (should (registers/flag-set? @registers :zero))
              (should (registers/flag-set? @registers :carry))
              (should (registers/flag-set? @registers :half-carry)))

          (it "should unset the operation flag"
             (should-not (registers/flag-set? @registers :operation)))) 

(describe "the add-registers-and-carry instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x3a :b 0xc5)
                                     (registers/set-flags :carry      true
                                                          :operation  true))
                              (ops/execute (ops/add-registers-and-carry :a :b))
                              :registers))
          (it "should add the values and store them in the first register"
              (should= 0 (@registers :a)))

          (it "should set the zero, carry, and half-carry flags"
              (should (registers/flag-set? @registers :zero))
              (should (registers/flag-set? @registers :carry))
              (should (registers/flag-set? @registers :half-carry)))

          (it "should unset the operation flag"
             (should-not (registers/flag-set? @registers :operation))))

(describe "the add-from-registers-address instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x3a
                                            :h 0x09
                                            :l 0x08)
                                     (registers/set-flag :operation))
                              (->/in [:memory]
                                     (memory/store 0x0908 0xc6))
                              (ops/execute (ops/add-from-registers-address :a :h :l))
                              :registers))

          (it "should add the values and store them in the destination register"
              (should= 0 (@registers :a)))

          (it "should set the zero, carry, and half-carry flags"
              (should (registers/flag-set? @registers :zero))
              (should (registers/flag-set? @registers :carry))
              (should (registers/flag-set? @registers :half-carry)))

          (it "should unset the operation flag"
             (should-not (registers/flag-set? @registers :operation))))

(describe "the add-from-registers-address-and-carry instruction"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :a 0x3a
                                            :h 0x09
                                            :l 0x08)
                                     (registers/set-flag :operation)
                                     (registers/set-flag :carry))
                              (->/in [:memory]
                                     (memory/store 0x0908 0xc5))
                              (ops/execute (ops/add-from-registers-address-and-carry :a :h :l))
                              :registers))

          (it "should add the values and store them in the destination register"
              (should= 0 (@registers :a)))

          (it "should set the zero, carry, and half-carry flags"
              (should (registers/flag-set? @registers :zero))
              (should (registers/flag-set? @registers :carry))
              (should (registers/flag-set? @registers :half-carry)))

          (it "should unset the operation flag"
              (should-not (registers/flag-set? @registers :operation))))
