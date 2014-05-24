(ns bar.ops.translators-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not should-throw with]]
                   [lonocloud.synthread :as ->]
                   [bar.ops.translators :refer [LD INC DEC]])
  (:require [speclj.core]
            [clojure.data :as data]
            [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.ops :as ops]
            [bar.system :as system]))

(describe "LD"
          (it "should handle the load-to-registers form"
              (let [op (LD BC, d16)
                    registers (-> system/zeroed
                                  (->/in [:memory]
                                         (memory/store 0 2)
                                         (memory/store 1 3))
                                  (ops/execute op)
                                  :registers)]
                (should= 2 (registers :c))
                (should= 3 (registers :b))
                (should= 3 (registers :m))))

          (it "should handle the store-from-registers-address form"
              (let [op (LD (BC), A)
                    memory (-> system/zeroed
                               (->/in [:registers]
                                      (assoc :a 23
                                             :b 9
                                             :c 8))
                               (ops/execute op)
                               :memory)]
                (should= 23 (memory/load memory 0x0908))))

          (it "should handle the load-immediate-value form"
              (let [registers (-> system/zeroed
                                  (->/in [:memory]
                                         (memory/store 0 2))
                                  (ops/execute (ops/load-immediate-value :b))
                                  :registers)]
                (should= 2 (registers :b))
                (should= 0x001 (registers :pc))))
          
          (with memory (-> system/zeroed
                           (assoc-in [:registers :sp] 0xfff8)
                           (->/in [:memory]
                                  (memory/store 0 0x00)
                                  (memory/store 1 0xc1))
                           (ops/execute (LD (d16), SP))
                           :memory))
          (it "should handle the store-stack-pointer form"
              (should= 0xf8 (memory/load @memory 0xc100))
              (should= 0xff (memory/load @memory 0xc101))))


(describe "INC"
          (it "should handle the increment-registers-address form"
              (let [op (INC BC)]
                (should= 2 (-> system/zeroed
                               (->/in [:registers]
                                      (assoc :b 9
                                             :c 8))
                               (->/in [:memory]
                                      (memory/store 0x0908 1))
                               (ops/execute op)
                               :memory
                               (memory/load 0x0908)))

                (should= 0 (-> system/zeroed
                               (->/in [:registers]
                                      (assoc :b 9
                                             :c 8))
                               (->/in [:memory]
                                      (memory/store 0x0908 0xff))
                               (ops/execute op)
                               :memory
                               (memory/load 0x0908)))))

          (it "should handle the increment-register form"
              (let [op (INC B)]
                (should= 2 (-> system/zeroed
                               (->/in [:registers]
                                      (assoc :b 1))
                               (ops/execute op)
                               :registers :b))
                (should= 0 (-> system/zeroed
                               (->/in [:registers]
                                      (assoc :b 0xff))
                               (ops/execute op)
                               :registers :b)))))

(describe "DEC"
          (it "should handle the decrement-register form"
              (let [op (DEC B)]
                (should= 1 (-> system/zeroed
                               (->/in [:registers]
                                      (assoc :b 2))
                               (ops/execute op)
                               :registers :b))
                (should= 0xff (-> system/zeroed
                                  (->/in [:registers]
                                         (assoc :b 0))
                                  (ops/execute op)
                                  :registers :b)))))
