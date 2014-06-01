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

(describe "->signed-byte"
          (it "converts from two's complement representation"
              (should= 127 (util/->signed-byte 2r01111111))
              (should= 126 (util/->signed-byte 2r01111110))
              (should= 2 (util/->signed-byte 2r00000010))
              (should= 1 (util/->signed-byte 2r00000001))
              (should= 0 (util/->signed-byte 2r00000000))
              (should= -1 (util/->signed-byte 2r11111111))
              (should= -2 (util/->signed-byte 2r11111110))
              (should= -126 (util/->signed-byte 2r10000010))
              (should= -127 (util/->signed-byte 2r10000001))
              (should= -128 (util/->signed-byte 2r10000000))))
