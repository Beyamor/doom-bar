(ns bar.system-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not with]]
                   [lonocloud.synthread :as ->])
  (:require [speclj.core]
            [bar.memory :as memory]
            [bar.system :as s]))

(describe "read-next-byte"
          (with result (-> s/zeroed
                           (->/in [:memory]
                                  (memory/store 0 2))
                           s/read-next-byte))
          (it "should return the memory value"
              (should= 2 (first @result)))
          (it "should increment the program counter"
              (should= 1 (-> @result second :registers :pc))))

(describe "store-in-memory"
          (it "should store the value in memory"
              (should= 2 (-> s/zeroed
                             ((s/store-in-memory 1 2))
                             second :memory (memory/load 1)))))

(describe "set-registers"
          (with registers (-> s/zeroed
                                ((s/set-registers :a 0xab
                                                  :b 0xcd))
                                second :registers))
          (it "should set the given registers"
              (should= 0xab (:a @registers))
              (should= 0xcd (:b @registers))))

(describe "update-register"
          (with result (-> s/zeroed
                             ((s/update-register :a inc))))
          (it "should update the register"
              (should= 1 (-> @result second :registers :a))))
