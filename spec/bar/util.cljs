(ns bar.util-spec
  (:require-macros [speclj.core :refer [describe it should should= should-not]])
  (:require [speclj.core]
            [bar.util :as util]))

(describe "in-range?"
          (it "checks if something is in a range, inclusive"
              (should (util/in-range? 2 1 3))
              (should (util/in-range? 1 1 3))
              (should (util/in-range? 3 1 3))
              (should-not (util/in-range? 0 1 3))
              (should-not (util/in-range? 4 1 3))))
