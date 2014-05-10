(ns bar.bit-spec
  (:require-macros [speclj.core :refer [describe it should should-not should=]])
  (:require [speclj.core]
            [bar.bit :as bit]))

(describe "set?"
          (it "should check for a set bit"
              (should (bit/set? 2r0101 0))
              (should (bit/set? 2r0101 2))

          (it "should also check for not set bits?"
              (should-not (bit/set? 2r0101 1)))
              (should-not (bit/set? 2r0101 3))))
