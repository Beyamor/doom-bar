(ns bar.ops-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not]])
  (:require [speclj.core]
            [bar.registers :as registers]
            [bar.bit :as bit]
            [bar.ops :as ops]))

(describe "addr-e"
          (it "should add registers a and e"
              (should= 4
                       (-> registers/zeroed
                           (assoc :e 4)
                           (ops/apply ops/addr-e)
                           :a)))

          (it "should set the time taken"
              (let [registers (-> registers/zeroed
                                 (assoc :e 4)
                                 (ops/apply ops/addr-e))]
                (should= 1 (:m registers))
                (should= 4 (:t registers))))

          (let [overflown-registers (-> registers/zeroed
                                       (assoc :e 260)
                                       (ops/apply ops/addr-e))]
            (it "should truncate to 8 bits"
                (should= 4 (:a overflown-registers)))

            (it "should set the carry bit"
                (should (registers/flag-set? overflown-registers :carry))))

          (it "should set the half-carry bit"
              (should (-> registers/zeroed
                          (assoc :a 0x8)
                          (assoc :e 0x8)
                          (ops/apply ops/addr-e)
                          (registers/flag-set? :half-carry)))

              (should-not (-> registers/zeroed
                          (assoc :a 0x7)
                          (assoc :e 0x8)
                          (ops/apply ops/addr-e)
                          (registers/flag-set? :half-carry))))

          (it "should set the zero flag"
              (should (-> registers/zeroed
                          (ops/apply ops/addr-e)
                          (registers/flag-set? :zero)))

              (should (-> registers/zeroed
                          (assoc :a 0x1)
                          (assoc :e 0xff)
                          (ops/apply ops/addr-e)
                          (registers/flag-set? :zero)))))
