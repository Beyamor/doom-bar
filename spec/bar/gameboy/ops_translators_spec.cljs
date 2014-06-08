(ns bar.gameboy.ops-translators-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not should-throw with]]
                   [lonocloud.synthread :as ->]
                   [bar.gameboy.ops-translators :refer [LD INC DEC ADD JR]])
  (:require [speclj.core]
            [clojure.data :as data]
            [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.ops :as ops]
            [bar.system :as system]))

(describe "LD"
          (with registers (-> system/zeroed
                              (->/in [:memory]
                                     (memory/store 0 2)
                                     (memory/store 1 3))
                              (ops/execute (LD BC, d16))
                              :registers))
          (it "should handle the load-to-registers form"
              (should= 2 (@registers :c))
              (should= 3 (@registers :b))
              (should= 3 (@registers :m)))

          (with memory (-> system/zeroed
                           (->/in [:registers]
                                  (assoc :a 23
                                         :b 9
                                         :c 8))
                           (ops/execute (LD (BC), A))
                           :memory))
          (it "should handle the store-from-registers-address form"
              (should= 23 (memory/load @memory 0x0908)))

          (it "should handle the load-from-registers-address form"
              (should= 0x2f (-> system/zeroed
                                (->/in [:memory]
                                       (memory/store 0x1234 0x2f))
                                (->/in [:registers]
                                       (assoc :b 0x12
                                              :c 0x34))
                                (ops/execute (LD A, (BC)))
                                :registers :a)))

          (with registers2 (-> system/zeroed
                               (->/in [:memory]
                                      (memory/store 0 2))
                               (ops/execute (ops/load-immediate-value :b))
                               :registers))
          (it "should handle the load-immediate-value form"
              (should= 2 (@registers2 :b))
              (should= 0x001 (@registers2 :pc)))

          (with memory2 (-> system/zeroed
                            (assoc-in [:registers :sp] 0xfff8)
                            (->/in [:memory]
                                   (memory/store 0 0x00)
                                   (memory/store 1 0xc1))
                            (ops/execute (LD (a16), SP))
                            :memory))
          (it "should handle the store-stack-pointer form"
              (should= 0xf8 (memory/load @memory2 0xc100))
              (should= 0xff (memory/load @memory2 0xc101)))

          (let [system (-> system/zeroed
                           (->/in [:registers]
                                  (assoc :a 0x56
                                         :h 0xff
                                         :l 0xff))
                           (ops/execute (LD (HL+), A)))]
            (it "should handle the store-from-registers-address-and-increment  form"
                (should= 0x56 (-> system :memory (memory/load 0xffff)))
                (should= 0 (-> system :registers :h))
                (should= 0 (-> system :registers :l)))))

(describe "INC"
          (it "should handle the increment-registers-address form"
              (should= 2 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 9
                                           :c 8))
                             (->/in [:memory]
                                    (memory/store 0x0908 1))
                             (ops/execute (INC BC))
                             :memory
                             (memory/load 0x0908)))
              (should= 0 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 9
                                           :c 8))
                             (->/in [:memory]
                                    (memory/store 0x0908 0xff))
                             (ops/execute (INC BC))
                             :memory
                             (memory/load 0x0908))))

          (it "should handle the increment-register form"
              (should= 2 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 1))
                             (ops/execute (INC B))
                             :registers :b))
              (should= 0 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 0xff))
                             (ops/execute (INC B))
                             :registers :b))))

(describe "DEC"
          (it "should handle the decrement-register form"
              (should= 1 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 2))
                             (ops/execute (DEC B))
                             :registers :b))
              (should= 0xff (-> system/zeroed
                                (->/in [:registers]
                                       (assoc :b 0))
                                (ops/execute (DEC B))
                                :registers :b)))

          (it "should handle the decrement-registers-address form"
              (should= 1 (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 9
                                           :c 8))
                             (->/in [:memory]
                                    (memory/store 0x0908 2))
                             (ops/execute (DEC BC))
                             :memory
                             (memory/load 0x0908)))
              (should= 0xff (-> system/zeroed
                             (->/in [:registers]
                                    (assoc :b 9
                                           :c 8))
                             (->/in [:memory]
                                    (memory/store 0x0908 0))
                             (ops/execute (DEC BC))
                             :memory
                             (memory/load 0x0908)))))

(describe "ADD"
          (with registers (-> system/zeroed
                              (->/in [:registers]
                                     (assoc :h 0x8a
                                            :l 0x23
                                            :b 0x06
                                            :c 0x05)
                                     (registers/unset-flag :operation))
                              (ops/execute (ADD HL, BC))
                              :registers))
          (it "should handle the add-register-words form"
              (should= 0x90 (@registers :h))
              (should= 0x28 (@registers :l))))

(describe "JR"
          (let [jump (fn [starting-address offset]
                       (-> system/zeroed
                           (->/in [:registers]
                                  (assoc :pc starting-address))
                           (->/in [:memory]
                                  (memory/store starting-address offset))
                           (ops/execute (JR r8))
                           :registers :pc))]
            (it "should handle the immediate-relative-jump form"
                (should= 201 (jump 150 50))
                (should= 101 (jump 150 -50))
                (should= (+ 0xff 50 1) (jump 0xff 50))
                (should= (+ 0xffff -50 2) (jump 0 -50))))

          (let [jump (fn [starting-address offset required-flags actual-flags]
                       (-> system/zeroed
                           (->/in [:registers]
                                  (assoc :pc starting-address)
                                  (#(reduce registers/set-flag % actual-flags)))
                           (->/in [:memory]
                                  (memory/store starting-address offset))
                           (ops/execute (JR Z, r8))
                           :registers :pc))]
            (it "should handle the conditional-relative-jump form"
                (should= 201 (jump 150 50 [:zero] [:zero]))
                (should= 101 (jump 150 -50 [:zero] [:zero]))
                (should= (+ 0xff 50 1) (jump 0xff 50 [:zero] [:zero]))
                (should= (+ 0xffff -50 2) (jump 0 -50 [:zero] [:zero]))
                (should= 151 (jump 150 50 [:zero] []))
                (should= 151 (jump 150 -50 [:zero] []))
                (should= 0x100 (jump 0xff 50 [:zero] []))
                (should= 1 (jump 0 -50 [:zero] [])))))
