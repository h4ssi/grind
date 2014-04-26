(ns grind.core
  (:require [clojure.browser.repl :as repl]
            [cljs.core.async :refer [chan sliding-buffer >! <!]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(defn ctx []
  (.. js/document (getElementById "grind") (getContext "2d")))

(def drawn (chan (sliding-buffer 1))) 
(def pressed (chan))
(def released (chan))

(defn draw [x]
  (let [c (ctx)]
    (.clearRect c 0 0 200 200)
    (set! (.-fillStyle c) "rgb(200,0,0)")
    (.fillRect c (+ 10 x) 10 100 100)))

(defn frame [t] ; t in millis
  ;(.log js/console t)

  (draw (* 10 (.sin js/Math (* 0.001 t))))
  (go (>! drawn t)))

(def initial-state {:x 100 :y 100})

(defn game-loop []
  (go (loop [state initial-state]
        (alt! [drawn] (.requestAnimationFrame js/window frame)
              [pressed] nil
              [released] nil)
        (recur state))))

(defn ^:export start []
  (repl/connect "http://localhost:9000/repl")
  (game-loop)
  (go (>! drawn 0)))

(defn ^:export keydown [e]
  (.preventDefault e)
  ;(.log js/console e)
  (go (>! pressed e)))

(defn ^:export keyup [e]
  (.preventDefault e)
  ;(.log js/console e)
  (go (>! released e)))
