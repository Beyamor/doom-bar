(ns bar.register-spec 
  (:require-macros [speclj.core :refer [describe it should should=]])
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
