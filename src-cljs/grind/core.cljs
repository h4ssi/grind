(ns grind.core
  (:require [clojure.browser.repl :as repl]
            [cljs.core.async :refer [chan sliding-buffer >! <!]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(defn l [i]
  (.log js/console (str i)) i)

(defn ctx []
  (.. js/document (getElementById "grind") (getContext "2d")))

(def drawn (chan (sliding-buffer 1))) 
(def pressed (chan))
(def released (chan))

(defn draw [state x]
  (let [c (ctx)]
    (.clearRect c 0 0 2000 2000)
    (set! (.-fillStyle c) "rgb(200,0,0)")
    (.fillRect c (+ (get-in state [:pos :x]) x) (get-in state [:pos :y]) 100 100)))

(defn render-callback [state]
  (fn [t] ; t in millis
    (draw state 0) ;(* 10 (.sin js/Math (* 0.001 t))))
    (go (>! drawn t))))

(def initial-state {:pos { :x 100 :y 100 }
                    :vel { :x 0   :y 0   }})

(def key-inputs {37 :left 
                 38 :up
                 39 :right
                 40 :down})

(defn key-input [inputs event kind]
  (let [code (.-keyCode event)]
    ;(l code)
    (if (contains? key-inputs code)
      (conj inputs [(get key-inputs code) kind])
      inputs)))

(defn key-pressed  [inputs event] (key-input inputs event :pressed))
(defn key-released [inputs event] (key-input inputs event :released))

(defn game-step [prev-state inputs]
  (when-not (empty? inputs) (l inputs))
  (let [change-vel        (fn [state axis dir pressed-or-released] 
                            (assoc-in state [:vel axis] (* (if (= pressed-or-released :pressed) 10 0) dir)))
        state-with-inputs (reduce (fn [state [input param]]
                                    (case input
                                      :left  (change-vel state :x -1 param)
                                      :right (change-vel state :x  1 param)
                                      :up    (change-vel state :y -1 param)
                                      :down  (change-vel state :y  1 param)))
                                  prev-state
                                  inputs)
        change-pos        (fn [axis state] (update-in state [:pos axis] #(+ % (get-in state [:vel axis]))))]
    (->> state-with-inputs
         (change-pos :x)
         (change-pos :y))))

(defn game-loop []
  (go (loop [[state inputs]
             [initial-state []]]
        (recur
          (alt! [drawn] (let [new-state (game-step state inputs)]
                          (.requestAnimationFrame js/window (render-callback new-state)) 
                          [new-state []])
                [pressed]  ([e _] [state (key-pressed  inputs e)]) 
                [released] ([e _] [state (key-released inputs e)]))))))

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
