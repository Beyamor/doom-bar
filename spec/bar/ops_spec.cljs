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
                              (ops/execute (ops/store-from-registers-address :b :c))
                              :memory
                              (memory/load 0x0908)))))

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
                              (ops/execute (ops/add-register-words :b :c))
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
                              (ops/execute (ops/add-register-words :h :l))
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
