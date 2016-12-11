(defproject kovasb/gamma-driver "auto"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories [["clojars" {:sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]]
  :plugins [[org.clojars.cvillecsteele/lein-git-version "1.0.0"]]
  :source-paths ["src"])
