(ns bar.registers-spec
  (:require-macros [speclj.core :refer [describe it should should= should-not]])
  (:require [speclj.core]
            [bar.registers :as registers]))

(describe "flags"
          (it "should be settable"
              (should= (:carry registers/flags)
                       (-> registers/zeroed
                           (registers/set-flag :carry)
                           :f)))

          (it "should be queriable"
              (should (-> registers/zeroed
                          (registers/set-flag :carry)
                          (registers/flag-set? :carry))))

          (it "should be unsettable"
              (let [registers (-> registers/zeroed
                                  (registers/set-flag :carry)
                                  (registers/set-flag :zero)
                                  (registers/unset-flag :carry))]
              (should-not (registers/flag-set? registers :carry))
              (should (registers/flag-set? registers :zero)))))

(describe "setting a map of flags"
          (it "should set truthy values"
              (let [registers (-> registers/zeroed
                                 (registers/set-flags :carry true :zero false))]
                (should (registers/flag-set? registers :carry)) 
                (should-not (registers/flag-set? registers :zero)) 
                (should-not (registers/flag-set? registers :half-carry))))

          (it "should unset falsey values"
              (let [registers (-> registers/zeroed
                                  (registers/set-flag :zero)
                                  (registers/set-flag :half-carry)
                                  (registers/set-flags :carry true :zero false))]
                (should (registers/flag-set? registers :carry)) 
                (should-not (registers/flag-set? registers :zero)) 
                (should (registers/flag-set? registers :half-carry)))))

(describe "address"
          (it "should produce the address pointed at by two registers"
              (should= 0x0908 (-> registers/zeroed
                                  (assoc :b 9 :c 8)
                                  (registers/address :b :c)))))
