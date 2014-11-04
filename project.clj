(defproject stagbeetle "0.1.0"
  :description "Battle animations of MOTHER 2"
  :url "http://github.com/subaru45/stagbeetle"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [seesaw "1.4.4"]]
  :main ^:skip-aot stagbeetle.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
