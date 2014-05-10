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
                          (registers/flag-set? :carry)))))

(describe "unset-flags"
          (it "should zero out the flags"
              (should= 0 (-> registers/zeroed
                             (registers/set-flag :carry)
                             (registers/unset-flags)
                             :f))))

(describe "setting a map of flags"
          (it "should set truthy values"
              (let [registers (-> registers/zeroed
                                 (registers/set-flags :carry true :zero false))]
                (should (registers/flag-set? registers :carry)) 
                (should-not (registers/flag-set? registers :zero)) 
                (should-not (registers/flag-set? registers :half-carry)) )))
