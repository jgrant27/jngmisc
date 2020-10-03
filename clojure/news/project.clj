(defproject news "0.1.0"
  :description "Simple concurrent HN news client"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "1.3.610"]
                 [clj-http "3.10.2"]
                 [cheshire "5.10.0"]
                 [criterium "0.4.6"]]
  :main ^:skip-aot news.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
