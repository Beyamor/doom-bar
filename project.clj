(defproject bar "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [lonocloud/synthread "1.0.4"]]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-2014"]
                                  [speclj "3.0.0"]]}}
  :plugins [[speclj "3.0.0"]
            [lein-cljsbuild "1.0.2"]]

  :cljsbuild {:builds        {:dev  {:source-paths   ["src" "spec"]
                                     :compiler       {:output-to     "js/bar_dev.js"
                                                      :optimizations :whitespace
                                                      :pretty-print  true}
                                     :notify-command ["phantomjs"  "bin/speclj" "js/bar_dev.js"]}

                              :prod {:source-paths ["src"]
                                     :compiler     {:output-to     "js/bar.js"
                                                    :optimizations :simple}}}
              :test-commands {"test" ["phantomjs" "bin/speclj" "js/bar_dev.js"]}}

  :source-paths ["src"]
  :test-paths ["spec"])
