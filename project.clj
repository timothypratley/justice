(defproject justice "0.0.3-alpha"
  :description "Concise rule definition and query syntax for DataScript"
  :url "https://github.com/timothypratley/justice"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.520" :scope "provided"]
                 [datascript "0.18.2" :scope "provided"]
                 [meander/gamma "0.0.22"]]
  :profiles {:dev {:source-paths ["examples"]}}
  :repl-options {:init-ns justice.core})
