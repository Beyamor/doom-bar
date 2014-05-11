(ns bar.ops.translators-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not should-throw]]
                   [lonocloud.synthread :as ->]
                   [bar.ops.translators :refer [LD INC]])
  (:require [speclj.core]
            [clojure.data :as data]
            [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.bit :as bit]
            [bar.ops :as ops]
            [bar.system :as system :refer [set-registers]]))

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
                               (set-registers :a 23
                                              :b 9
                                              :c 8)
                               (ops/execute op)
                               :memory)]
                (should= 23 (memory/load memory 0x0908)))))

(describe "INC"
          (it "should handle the increment-registers-address form"
              (let [op (INC BC)]
                (should= 2 (-> system/zeroed
                               (set-registers :b 9
                                              :c 8)
                               (->/in [:memory]
                                      (memory/store 0x0908 1))
                               (ops/execute op)
                               :memory
                               (memory/load 0x0908)))

                (should= 0 (-> system/zeroed
                               (set-registers :b 9
                                              :c 8)
                               (->/in [:memory]
                                      (memory/store 0x0908 0xff))
                               (ops/execute (ops/increment-registers-address :b :c))
                               :memory
                               (memory/load 0x0908))))))
