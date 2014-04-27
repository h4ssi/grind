(ns grind.core
  (:require [clojure.browser.repl :as repl]
            [cljs.core.async :refer [chan sliding-buffer >! <!]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(defn l [i]
  (.log js/console (str i)) i)

; map

(defn random-seed [seed] 
  (let [seedrandom (.-seedrandom js/Math)] (seedrandom. seed)))

(defn random-int  [rng from to] (+ (.floor js/Math (* (rng) (inc (- to from)))) from))

(defn random-elem [rng elems] (get elems (random-int rng 0 (dec (count elems)))))

(def dirs [:left :right :up :down])

(def opposit {:left :right
              :right :left
              :up :down
              :down :up})

(defn lvl-paths [seed max-level]
  (let [rng  (random-seed seed)
        tabu (fn [d1 d2] (filterv #(not (contains? #{d1 d2} %)) dirs))
        path (fn [general-dir] 
               (loop [p   [general-dir]
                      lvl (assoc (apply hash-map (flatten (map #(vector % 0) dirs))) general-dir 1)
                      prev general-dir]
                 (let [step (random-elem rng (tabu (general-dir opposit) (prev opposit)))]
                   (if (= (dec max-level) (step lvl))
                     (conj p step)
                     (recur (conj p step) (update-in lvl [step] inc) step)))))]
    (mapv #(path %) dirs))) 

(defn move-coord [x y dir] 
  (case dir
    :left  [(dec x) y]
    :right [(inc x) y]
    :up    [x (dec y)]
    :down  [x (inc y)]))

        
(defn lvl-gate-coords [paths]
  (let [add-connected  (fn [coords path-num x y prev-dir dir]
                         (assoc-in coords [[x y] path-num] [(prev-dir opposit :mid) dir])) 

        path-to-coords (fn [path-num init-coords]
                         (loop [coords init-coords
                                [x y] [0 0]
                                prev-dir :mid
                                [dir & path] (get paths path-num)]
                           (if dir
                             (recur (add-connected coords path-num x y prev-dir dir)
                                    (move-coord x y dir)
                                    dir
                                    path)
                             (add-connected coords path-num x y prev-dir :mid))))]

    (loop [[i & is] (range (count paths))
           coords {}]
      (if i
        (recur is (path-to-coords i coords))
        coords)))) 

(defn get-gates [coords x y & options]
  (let [borders            (into #{} (concat [:down :right] options)) ; chunk (x,y) is responsible for, down right and mid (via options)
        coord              (get coords [x y])
        filter-down-rights (partial filterv #(contains? borders %)) 
        down-rights        (into {} (for [[path-num connection] coord] 
                                      (let [filtered (filter-down-rights connection)]
                                        (if (empty? filtered)
                                          nil
                                          [path-num filtered]))))]
    down-rights))

(defn rng-for [seed x y] (random-seed (str seed \  x \  y)))

(def map-chunk-size 101)

(defn get-gates-positions [gates rng]
  (into {} (for 
             [[path-num borders] gates] 
             [path-num (into {} (map #(vector % [(random-int rng 0 (dec map-chunk-size))]) borders))])))

(defn map-chunk [seed coords x y]
  (let [gates-positions (fn [n-x n-y & with-mid?] (let [rng       (rng-for seed n-x n-y)
                                                        gates     (apply get-gates coords n-x n-y with-mid?)
                                                        positions (get-gates-positions gates rng)]
                                                    [positions rng]))

        dir-to-pos       (fn [dir] (case dir
                                     :left  0
                                     :right (dec map-chunk-size)
                                     :up    0
                                     :down  (dec map-chunk-size)
                                     :mid   (quot map-chunk-size 2)))

        absolute-pos     (fn [dir pos] (case dir
                                         (:left :right) [(dir-to-pos dir) pos]
                                         (:up   :down)  [pos (dir-to-pos dir)]
                                         :mid           [(dir-to-pos :mid) (dir-to-pos :mid)]))

        absolute-for-dir (fn [dir-inner dir-outer borders] (when (contains? borders dir-outer)
                                                             (mapv (partial absolute-pos dir-inner) (dir-outer borders))))

        absolute-gates   (fn [dir-inner dir-outer positions]
                           (into {} (for [[path-num borders] positions] [path-num (absolute-for-dir dir-inner dir-outer borders)])))

        neighbor-gates   (fn [dir] (let [[n-x n-y]     (move-coord x y dir)
                                         [positions _] (gates-positions n-x n-y)]
                                     (absolute-gates dir (dir opposit) positions)))

        [positions rng]  (gates-positions x y :mid)

        my-gates         (fn [dir] (absolute-gates dir dir positions))

        all-gates        (merge-with concat (my-gates :mid) (my-gates :down) (my-gates :right) (neighbor-gates :up) (neighbor-gates :left))

        route            (fn [sx sy tx ty] (let [[ssx ttx] (sort [sx tx])
                                                 [ssy tty] (sort [sy ty])]
                                             (apply concat (for [x (range ssx (inc ttx))
                                                                 y (range ssy (inc tty))] [[x sy] [tx y]]))))

        all-routes       (into #{} 
                               (apply concat (for [[path gates] all-gates 
                                             :when path 
                                             :when gates] 
                                               (let [[g1 g2] gates] (apply route (concat g1 g2))))))]
    all-routes))


; game loop + graphics

(def block-size 30)
(def map-chunk-texture-width (* block-size map-chunk-size))
(def player-size 10)

(defn render-canvas [] (.. js/document (getElementById "grind")))

(defn render-context 
  ([elem] (.. elem (getContext "2d")))
  ([]     (render-context (render-canvas))))

(def drawn (chan (sliding-buffer 1))) 
(def pressed (chan))
(def released (chan))

(defn draw-map-chunk [map-chunk]
  (let [canvas (.. js/document (createElement "canvas"))
        size   (* block-size map-chunk-size)]
    (set! (.-width  canvas) size)
    (set! (.-height canvas) size)
    (let [c (render-context canvas)]
      (dorun (for [x (range map-chunk-size)
                   y (range map-chunk-size)]
               (when-not (contains? map-chunk [x y])
                 (set! (.-fillStyle c) (if (even? (+ x y)) "rgb(0,0,200)" "rgb(0,0,100"))
                 (.fillRect c (* x block-size) (* y block-size) block-size block-size))))
      canvas)))

(defn offset [size] (quot size -2))

(defn draw [canvas context state]
  (let [c             context
        px            (get-in state [:pos :x])
        py            (get-in state [:pos :y])
        [cx cy]       (:chunk-index state)
        w             (.-width  canvas)
        h             (.-height canvas)
        tx            (quot (.-width  canvas) 2)
        ty            (quot (.-height canvas) 2)
        player-offset (offset player-size)
        chunk-offset  (offset map-chunk-texture-width)]
    (.clearRect c 0 0 w h)
    (.save c)
    (.translate c (- tx px) (- ty py))
    ; draw map
    (.drawImage c (:chunk-texture state) (+ (* cx map-chunk-texture-width) chunk-offset) (+ (* cy map-chunk-texture-width) chunk-offset))
    ; draw player
    (set! (.-fillStyle c) "rgb(200,0,0)")
    (.translate c player-offset player-offset)
    (.fillRect c px  py player-size player-size)
    (.restore c)))

(defn render-callback [canvas context state]
  (fn [t] ; t in millis
    (draw canvas context state) ;(* 10 (.sin js/Math (* 0.001 t))))
    (go (>! drawn t))))

(defn pos-to-chunk-index [x y]
  (let [o (offset map-chunk-texture-width)
        t (fn [c] (.ceil js/Math (double (/ (+ c o) map-chunk-texture-width))))]
    [(t x) (t y)]))

(defn load-chunk [state x y]
  (let [chunk (map-chunk "" (grind.core/lvl-gate-coords (grind.core/lvl-paths "" 3)) x y)]
    (l (str "loading chunk " x \/ y))
    (assoc state
           :chunk-index [x y]
           :chunk chunk
           :chunk-texture (draw-map-chunk chunk))))

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

(def initial-state {:pos { :x 0 :y 0 }
                    :vel { :x 0 :y 0 }})

(defn game-step [prev-state inputs]
  ;(when-not (empty? inputs) (l inputs))
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
        change-pos        (fn [axis state] (update-in state [:pos axis] #(+ % (get-in state [:vel axis]))))
        check-new-chunk   (fn [state]
                            (let [chunk-index (pos-to-chunk-index (get-in state [:pos :x]) (get-in state [:pos :y]))]
                              (if (= (:chunk-index state) chunk-index)
                                state
                                (apply load-chunk state chunk-index))))]
    (->> state-with-inputs
         (change-pos :x)
         (change-pos :y)
         (check-new-chunk))))

(defn game-loop []
  (let [canvas  (render-canvas)
        context (render-context)]
    (go (loop [[state inputs]
               [initial-state []]]
          (recur
            (alt! [drawn] (let [new-state (game-step state inputs)]
                            (.requestAnimationFrame js/window (render-callback canvas context new-state)) 
                            [new-state []])
                  [pressed]  ([e _] [state (key-pressed  inputs e)]) 
                  [released] ([e _] [state (key-released inputs e)])))))
    (go (>! drawn :start))))

(defn ^:export start []
  (repl/connect "http://localhost:9000/repl")
  (game-loop))

(defn ^:export keydown [e]
  (.preventDefault e)
  ;(.log js/console e)
  (go (>! pressed e)))

(defn ^:export keyup [e]
  (.preventDefault e)
  ;(.log js/console e)
  (go (>! released e)))
