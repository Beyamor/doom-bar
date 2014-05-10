(ns bar.system-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not]])
  (:require [speclj.core]
            [bar.system :as system]))

(describe "setting the registers"
          (it "should be possible to set one"
              (should= 23
                       (-> system/zeroed
                           (system/set-register :a 23)
                           :registers
                           :a)))

          (it "should be possible to set several"
              (let [registers (-> system/zeroed
                                  (system/set-registers :a 23 :b 24)
                                  :registers)]
                (should= 23 (:a registers))
                (should= 24 (:b registers)))))
