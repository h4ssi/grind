(ns grind.core
  (:require [clojure.browser.repl :as repl]))

(defn ^:export greet [n]
  (str "Hello " n))

(repl/connect "http://localhost:9000/repl")
