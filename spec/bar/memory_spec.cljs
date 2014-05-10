(ns bar.memory-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not]])
  (:require [speclj.core]
            [bar.memory :as memory]))

(describe "memory"
          (it "should be 64k"
              (should= 65536
                       (count memory/zeroed)))

          (it "can be written to and read from"
              (should= 23
                       (-> memory/zeroed
                           (memory/store 0xC000 23)
                           (memory/load 0xC000))))

          (it "shadows working [0xC000-0xDDFF] in [0xE000-0xFDFF]"
              (should= 23 
                       (-> memory/zeroed
                           (memory/store 0xC000 23)
                           (memory/load 0xE000)))

              (should= 23
                       (-> memory/zeroed
                           (memory/store 0xE000 23)
                           (memory/load 0xC000)))))
