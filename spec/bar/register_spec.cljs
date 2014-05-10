(ns bar.register-spec 
  (:require-macros [speclj.core :refer [describe it should should= should-not]])
  (:require [speclj.core]
            [bar.register :as register]))

(describe "flags"
          (it "should be settable"
              (should= (:carry register/flags)
                       (-> register/zeroed
                           (register/set-flag :carry)
                           :f)))

          (it "should be queriable"
              (should (-> register/zeroed
                          (register/set-flag :carry)
                          (register/flag-set? :carry)))))

(describe "unset-flags"
          (it "should zero out the flags"
              (should= 0 (-> register/zeroed
                             (register/set-flag :carry)
                             (register/unset-flags)
                             :f))))

(describe "setting a map of flags"
          (it "should set truthy values"
              (let [register (-> register/zeroed
                                 (register/set-flags :carry true :zero false))]
                (should (register/flag-set? register :carry)) 
                (should-not (register/flag-set? register :zero)) 
                (should-not (register/flag-set? register :half-carry)) )))
