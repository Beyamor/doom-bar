(ns bar.memory-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not]])
  (:require [speclj.core]
            [bar.memory :as memory]))

(describe "memory"
          (it "should be 64k"
              (should= 65536
                       (count memory/zeroed))))
