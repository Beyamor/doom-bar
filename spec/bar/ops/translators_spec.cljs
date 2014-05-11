(ns bar.ops.translators-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not should-throw]]
                   [lonocloud.synthread :as ->]
                   [bar.ops.translators :refer [ld]])
  (:require [speclj.core]
            [clojure.data :as data]
            [bar.registers :as registers]
            [bar.memory :as memory]
            [bar.bit :as bit]
            [bar.ops :as ops]
            [bar.system :as system :refer [set-registers]]))

(describe "ld"
          (it "should handle the load-to-registers form"
              (let [op (ld bc, d16)
                    registers (-> system/zeroed
                                  (->/in [:memory]
                                         (memory/store 0 2)
                                         (memory/store 1 3))
                                  (ops/execute op)
                                  :registers)]
                (should= 2 (registers :c))
                (should= 3 (registers :b))
                (should= 3 (registers :m)))))
