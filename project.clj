(defproject de.otto/nom "0.2.1-SNAPSHOT"
  :description "Utilities for working with the happy path in the face of anomalies."
  :url "https://github.com/otto-de/nom"
  :license {:name "Apache License 2.0"
            :url  "http://www.apache.org/license/LICENSE-2.0.html"}
  :dependencies [[org.clojure/clojure "1.10.3" :scope "provided"]]
  :source-paths ["src/cljc"]
  :test-paths ["test/cljc"]
  :cljfmt {:indents {let-nom [[:inner 0]]
                     let-nom> [[:inner 0]]
                     try-nom [[:inner 0]]
                     with-nom [[:inner 0]]}})
