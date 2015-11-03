(defproject sol "0.0.1"
  :description "Implementation of Sol in Clojure"
  :url "http://github.com/prismofeverything"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [http-kit "2.1.12"]
                 [polaris "0.0.2"]
                 [ring "1.2.0"]
                 [org.clojure/clojurescript "0.0-1909"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]
                 [domina "1.0.2"]
                 [com.keminglabs/singult "0.1.6"]
                 [org.clojure/google-closure-library-third-party "0.0-2029"]]
  :plugins [[lein-cljsbuild "1.0.0"]]
  :source-paths ["src/clj"]
  :resource-paths ["resources/"]
  :min-lein-version "2.0.0"
  :repl-options {:host "localhost"
                 :port 11911}
  :main sol.server
  :cljsbuild 
  {:builds 
   {:dev 
    {:libs ["singult"]
     :source-paths ["src/cljs"]
     :compiler
     {:externs ["resources/public/js/externs/snap.svg.externs.js"]
      :optimizations :whitespace
      :output-to  "resources/public/js/app/sol.js"
      :output-dir "resources/public/js/app/out"}}}})
