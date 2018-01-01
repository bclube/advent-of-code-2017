(defproject advent-of-code-2017 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-http "3.7.0"]
                 [environ "1.1.0"]]
  :test-selectors {:default (complement :slow)}
  :main ^:skip-aot advent-of-code-2017.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
