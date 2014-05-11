(ns bar.ops-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not should-throw]]
                   [lonocloud.synthread :as ->])
  (:require [speclj.core]
            [clojure.data :as data]
            [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.bit :as bit]
            [bar.ops :as ops]
            [bar.system :as system :refer [set-registers]]))

(describe "no-op"
          (it "does nothing except increment the time"
              (let [[_ diff _] (data/diff
                                 system/zeroed
                                 (ops/execute system/zeroed ops/no-op))]
                (should= diff
                         {:registers {:m 1}}))))
              

(describe "an addr instruction"
          (it "should add registers a and e"
              (should= 4
                       (-> system/zeroed
                           (set-registers :e 4)
                           (ops/execute (ops/addr :e))
                           :registers :a)))

          (it "should set the time taken"
              (let [registers (-> system/zeroed
                                  (set-registers :e 4)
                                  (ops/execute (ops/addr :e))
                                  :registers)]
                (should= 1 (:m registers))))

          (let [overflown-registers (-> system/zeroed
                                        (set-registers :a 255
                                                       :e 5)
                                        (ops/execute (ops/addr :e))
                                        :registers)]
            (it "should truncate to 8 bits"
                (should= 4 (:a overflown-registers)))

            (it "should set the carry bit"
                (should (registers/flag-set? overflown-registers :carry))))

          (it "should set the half-carry bit"
              (should (-> system/zeroed
                          (set-registers :a 0x8
                                         :e 0x8)
                          (ops/execute (ops/addr :e))
                          :registers
                          (registers/flag-set? :half-carry)))

              (should-not (-> system/zeroed
                              (set-registers :a 0x7
                                             :e 0x8)
                              (ops/execute (ops/addr :e))
                              :registers
                              (registers/flag-set? :half-carry))))

          (it "should set the zero flag"
              (should (-> system/zeroed
                          (ops/execute (ops/addr :e))
                          :registers
                          (registers/flag-set? :zero)))

              (should (-> registers/zeroed
                          (set-registers :a 0x1
                                         :e 0xff)
                          (ops/execute (ops/addr :e))
                          :registers
                          (registers/flag-set? :zero)))))

(describe "unimplemented-op"
          (it "should throw"
              (should-throw js/Error
                            (-> system/zeroed
                                (ops/execute ops/unimplemented-op)))))

(describe "a load to registers instruction"
          (let [registers (-> system/zeroed
                              (->/in [:memory]
                                     (memory/store 0 2)
                                     (memory/store 1 3))
                              (ops/execute (ops/load-to-registers :b :c))
                              :registers)]
            (it "should set the register values"
                (should= 2 (registers :c))
                (should= 3 (registers :b)))

            (it "should take three cycles"
                (should= 3 (registers :m)))))

(describe "a store from registers address instruction"
          (let [memory (-> system/zeroed
                           (set-registers :a 23
                                          :b 9
                                          :c 8)
                           (ops/execute (ops/store-from-registers-address :b :c))
                           :memory)]
            (it "should store a value in memory"
                (should= 23 (memory/load memory 0x0908)))))
