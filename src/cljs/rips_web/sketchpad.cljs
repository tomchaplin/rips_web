(ns rips-web.sketchpad
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as r]
            [re-frame.core :as re-frame]
            [rips-web.events :as events]
            [clojure.set :as cst]
            [reagent.dom :as rdom]))

(defn compute-grid-size [ep dims]
  (map #(Math/ceil (/ % ep)) dims))

(def OPTS
  (let [ep 200
        dims [2000 1000]]
    {:dimensions dims
     :wiggle-speed 2
     :volatility (/ 1 500)
     :num-points 30
     :max-dim 2
     :epsilon ep
     :epislon-sq (* ep ep)
     :grid-size (compute-grid-size ep dims)
     :target-fps 60}))

; UTILITY

(defn debug-print [x]
  (.log js/console x))

(defn my-rand
  ([] (my-rand 1))
  ([n] (* (Math/random) n)))

; Return true iff some n of coll satisfy pred
(defn some-n [n pred coll]
  (if (<= n 0)
    ; We have found all n so true
    true
    (if (empty? coll)
      ; We still have some to find and no-where to find them
      false
      ; We check the first element of collection
      (if (pred (first coll))
        (some-n (- n 1) pred (rest coll))
        (some-n n pred (rest coll))
        ))))

; SETUP

(defn rand-list [n [max-x max-y]]
  (let [rand-x (* max-x (my-rand))
        rand-y (* max-y (my-rand))]
  (if (= n 1)
    [[rand-x rand-y]]
    (vec (cons [rand-x rand-y] (rand-list (- n 1) [max-x max-y]) ))
    )))

(defn setup [opts]
  (fn [_]
    (q/frame-rate (:target-fps opts))
    {:complex [(rand-list (:num-points opts) (:dimensions opts))]
     :opts opts
     :lagging 0}
    ))

; DRAWING

(defn add-vertex [index points]
  (let [[x y] (nth points index)]
    (q/vertex x y)))

(defn draw-simplex [tau points]
  (q/begin-shape)
  (doall (map #(add-vertex % points) tau))
  (q/end-shape))

(defn draw-edge [[p1 p2] points]
  (let [[p1x p1y] (nth points p1)
        [p2x p2y] (nth points p2)]
    (q/line p1x p1y p2x p2y)))

(defn plot-point [[a b]]
  (q/ellipse a b 9 9))

(defn draw [state]
  (let [points (get-in state [:complex 0])
        edges  (get-in state [:complex 1])]
  (q/background 10 10 20)
  ; Draw simplices
  (q/fill 220 220 240 100)
  (doall (map #(draw-simplex % points) (get-in state [:complex 2])))
  ; Draw edges
  (q/stroke 220 220 240 220)
  (q/stroke-weight 3)
  (doall (map #(draw-edge % points) edges))
  ; Draw points
  (q/no-stroke)
  (q/fill 220 220 240 220)
  (doall (map plot-point points))
  (debug-print (q/current-frame-rate))
  ))

; MOVING AROUND POINTS

(defn loop-around [[x y] [max-x max-y]]
    [(mod x max-x) (mod y max-y)])

(defn noisy-velocity [t opts]
  (fn [i [a b]]
    (let [VOLATILITY (:volatility opts)
          WIGGLE-SPEED (:wiggle-speed opts)
          DIMENSIONS (:dimensions opts)
          noise_param (+ i  (* t VOLATILITY))
          x_noise (q/noise noise_param i)
          y_noise (q/noise i noise_param)]
    (loop-around 
      [(+ a (* WIGGLE-SPEED (- x_noise 0.5))) (+ b (* WIGGLE-SPEED (- y_noise 0.5)))]
      DIMENSIONS
      ))))

(defn peturb-points [state]
  (assoc-in state [:complex 0]
    (map-indexed
      (noisy-velocity (q/frame-count) (:opts state))
      (get-in state [:complex 0]))))

; NEIGHBOURHOOD GRAPH

(defn check-edge [[xi yi] points EPSILON_SQ]
  (let [[x1 x2] (nth points xi)
        [y1 y2] (nth points yi)
        d1 (- x1 y1)
        d2 (- x2 y2)
        distance2 (+ (* d1 d1) (* d2 d2)) ]
    (<= distance2 EPSILON_SQ)))

(defn get-grid-coords [coords EPSILON]
  (map #(Math/floor (/ % EPSILON)) coords ))

; TODO: Change to loop-recur rather than functional recursion?
(defn populate-grid [remaining-points i current-grid opts]
  (let [EPSILON (:epsilon opts)
        next-point (first remaining-points)
        grid-coords (get-grid-coords next-point EPSILON)]
  (if (empty? remaining-points) current-grid
    (populate-grid 
      (rest remaining-points)
      (+ i 1)
      (assoc-in current-grid grid-coords
                (cons i (get-in current-grid grid-coords)))
      opts))))

(defn compute-graph-grid [state]
  (let [opts (:opts state)
        [g1 g2] (:grid-size opts)
        points (get-in state [:complex 0])
        grid (vec (repeat g1 (vec (repeat g2 ()))))]
    (populate-grid points 0 grid opts)))

(defn grid-add [[x y] [a b]] [(+ x a) (+ y b)])

(defn valid-grid-coord [[i j] [g1 g2]]
  (let [i-valid (and (>= i 0) (< i g1))
        j-valid (and (>= j 0) (< j g2))]
    (and i-valid j-valid)))

; grid is now a 2-dim vector with each entry containing a list ()
; These are indices of points in that square
; o o *
; o c *
; o * *
; When working on square c we look for neighbours in c and *
(defn compute-graph [state]
  (let [opts (:opts state)
        MODIFIERS [[0 0] [0 1] [1 -1] [1 0] [1 1]]
        GRID_SIZE (:grid-size opts)
        EPSILON_SQ (:epislon-sq opts)
        grid (compute-graph-grid state)
        points (get-in state [:complex 0])
        [g1 g2] GRID_SIZE]
    (assoc-in state [:complex 1]
      ; TODO: Do I need to filter not-empty?
      (apply concat (filter not-empty
        (for [i (range 0 g1)
              j (range 0 g2)
              modifier (range 0 5)
              :let [coord1 [i j]
                    coord2 (grid-add coord1 (nth MODIFIERS modifier))]
              :when (valid-grid-coord coord2 GRID_SIZE)]
            (let [nodes1 (get-in grid coord1)
                  nodes2 (get-in grid coord2)]
              (remove nil? 
                (for [n1 nodes1
                      n2 nodes2
                      ; This removes self-loops and counting edges twice
                      :when (or (not= modifier 0) (< n1 n2))
                      :let [min-node (min n1 n2)
                            max-node (max n1 n2)]]
                  ; Check distance between p1 and p2
                  ; Include edge from smallest to largest node
                  (if 
                    (check-edge [n1 n2] points EPSILON_SQ)
                    [min-node max-node] nil))))))))))

; VR EXPANSION

; Returns whether {u}∪(tau) is a clique
; It is assume u is lower than all tau vertices
(defn would-form-clique [u tau edges]
  (let [k (count tau)
        required-edges (map #(vec [u %]) tau)]
    ; Check that k edges coincide with at least one of the required-edges
    ; These sequences have no duplicates and there are k required-edges
    ; Therefore we are checking requried-edges subset of edges
    ; TODO: Test converting to sets then usign subset?
    (some-n
      k
      #(some (partial = %) required-edges)
      edges)
    ))

(defn lower-neighbours [tau state]
  (let [edges (get-in state [:complex 1])
        lowest-v (apply min tau)]
    (filter 
      #(would-form-clique % tau edges)
      (range 0 lowest-v))))

;TODO: Change to loop-recur rather than functional recursion?
(defn add-simplices-up-to [i state]
  ; Should be receiving state with edges already complete
  (if (= i 1) state
    ; Populate lower-dim simplices
    (let [state (add-simplices-up-to (- i 1) state)
          lower-simplices (get-in state [:complex (- i 1)])]
      ; Now we add i-simplices
      (assoc-in state [:complex i]
        (apply concat (filter not-empty
        ; For each lower dim simplicies find all lower-neighbours
          (for [tau lower-simplices
                :let [ N (lower-neighbours tau state)]]
            ; Union each lower-neighbour to tau and add to simplex list
            (for [v N] (conj tau v) )
            )))))))

(defn vr-expand [state]
  (add-simplices-up-to (get-in state [:opts :max-dim]) state))

; CHANGE OPTIONS BASED ON PERFORMANCE

(defn performance-check [state]
  (let [opts (:opts state)
        dims (:dimensions opts)
        fps  (q/current-frame-rate)
        old-ep (:epsilon opts)
        lagging (:lagging state)
        new-lagging (if (< fps 25) (+ lagging 1) 0)]
    (if (>= new-lagging 25)
      ; If we've dropped below 25fps for 25 consecutive updates then change epsilon
      (let [new-ep (- old-ep 10)
            new-ep-sq (* new-ep new-ep)]
        (debug-print new-ep)
        (-> state
          (assoc-in ,,, [:opts]
              (merge opts {:epsilon new-ep
                           :epislon-sq new-ep-sq
                           :grid-size (compute-grid-size new-ep dims)}))
          (assoc-in ,,, [:lagging] 0)
          ))
      ; Else carry on
      (assoc-in state [:lagging] new-lagging)
      )))

; UPDATE FUNCTION

(defn update-state [state]
  (-> state
      (peturb-points ,,,)
      (compute-graph ,,,)
      (vr-expand     ,,,)
      ;(performance-check ,,,)
      ))

(defn canvas []
  (r/create-class
    {:component-did-mount
     (fn [component]
      (let [node (rdom/dom-node component)
            width (.-offsetWidth node)
            height (.-offsetHeight node)]
        (q/sketch
          :setup (setup OPTS)
          :update update-state
          :draw draw
          :host "canvas_wrapper"
          :size (:dimensions OPTS)
          :middleware [m/fun-mode])))
     :render
     (fn []
       [:div {:id "canvas_wrapper" :width "100%" :height "100%"}])}))
