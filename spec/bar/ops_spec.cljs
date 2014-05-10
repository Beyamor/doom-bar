(ns bar.ops-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not]])
  (:require [speclj.core]
            [bar.register :as register]
            [bar.bit :as bit]
            [bar.ops :as ops]))

(describe "addr-e"
          (it "should add registers a and e"
              (should= 4
                       (-> register/zeroed
                           (assoc :e 4)
                           (ops/apply ops/addr-e)
                           :a)))

          (it "should set the time taken"
              (let [register (-> register/zeroed
                                 (assoc :e 4)
                                 (ops/apply ops/addr-e))]
                (should= 1 (:m register))
                (should= 4 (:t register))))

          (let [overflown-register (-> register/zeroed
                                       (assoc :e 260)
                                       (ops/apply ops/addr-e))]
            (it "should truncate to 8 bits"
                (should= 4 (:a overflown-register)))

            (it "should set the carry bit"
                (should (register/flag-set? overflown-register :carry))))

          (it "should set the half-carry bit"
              (should (-> register/zeroed
                          (assoc :a 0x8)
                          (assoc :e 0x8)
                          (ops/apply ops/addr-e)
                          (register/flag-set? :half-carry)))

              (should-not (-> register/zeroed
                          (assoc :a 0x7)
                          (assoc :e 0x8)
                          (ops/apply ops/addr-e)
                          (register/flag-set? :half-carry))))

          (it "should set the zero flag"
              (should (-> register/zeroed
                          (ops/apply ops/addr-e)
                          (register/flag-set? :zero)))

              (should (-> register/zeroed
                          (assoc :a 0x1)
                          (assoc :e 0xff)
                          (ops/apply ops/addr-e)
                          (register/flag-set? :zero)))))
