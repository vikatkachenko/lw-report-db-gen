(defproject lw-report-db-gen "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha16"]
                 [org.clojure/test.check "0.9.0"]
                 [orchestra "2017.07.04-1"]
                 [com.rpl/specter "1.0.4"]
                 [org.clojure/data.csv "0.1.4"]
                 [clj-time "0.14.2"]]
  :main ^:skip-aot lw-report-db-gen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
