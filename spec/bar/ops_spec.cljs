(ns bar.ops-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not]]
                   [lonocloud.synthread :as ->])
  (:require [speclj.core]
            [bar.registers :as registers]
            [bar.bit :as bit]
            [bar.ops :as ops]
            [bar.system :as system :refer [set-registers]]))

(describe "addr-e"
          (it "should add registers a and e"
              (should= 4
                       (-> system/zeroed
                           (set-registers :e 4)
                           (ops/execute ops/addr-e)
                           :registers :a)))

          (it "should set the time taken"
              (let [registers (-> system/zeroed
                                  (set-registers :e 4)
                                  (ops/execute ops/addr-e)
                                  :registers)]
                (should= 1 (:m registers))
                (should= 4 (:t registers))))

          (let [overflown-registers (-> system/zeroed
                                        (set-registers :a 255
                                                       :e 5)
                                        (ops/execute ops/addr-e)
                                        :registers)]
            (it "should truncate to 8 bits"
                (should= 4 (:a overflown-registers)))

            (it "should set the carry bit"
                (should (registers/flag-set? overflown-registers :carry))))

          (it "should set the half-carry bit"
              (should (-> system/zeroed
                          (set-registers :a 0x8
                                         :e 0x8)
                          (ops/execute ops/addr-e)
                          :registers
                          (registers/flag-set? :half-carry)))

              (should-not (-> system/zeroed
                              (set-registers :a 0x7
                                             :e 0x8)
                              (ops/execute ops/addr-e)
                              :registers
                              (registers/flag-set? :half-carry))))

          (it "should set the zero flag"
              (should (-> system/zeroed
                          (ops/execute ops/addr-e)
                          :registers
                          (registers/flag-set? :zero)))

              (should (-> registers/zeroed
                          (set-registers :a 0x1
                                         :e 0xff)
                          (ops/execute ops/addr-e)
                          :registers
                          (registers/flag-set? :zero)))))
