(use freja/flow)


### UTIL

(defn circle-circle?
  [{:pos p1 :radius r1}
   {:pos p2 :radius r2}]
  (<= (v/dist p1 p2) (+ r1 r2)))

(defn array-copy
  [a1 a2]
  (array/clear a1)
  (loop [i :range [0 (length a2)]]
    (put a1 i (in a2 i)))
  a1)

(defn lerp
  [v1 v2 t]
  (+ v1 (* t (- v2 v1))))

(defn v-lerp
  [[x y] [x2 y2] v]
  [(lerp x x2 v) (lerp y y2 v)])

(defn rng
  [start stop]
  (lerp start stop (math/random)))

#### ACTUAL GAME

(def state
  @{:mp @[0 0]
    :player @{:pos @[10 10]
              :radius 10
              :speed 100
              :max-speed 5
              :traction 1
              :vel @[0 0]}
    :bullets @[]
    :selected-bullet :oil
    :in @[0 0]})

(defn shoot [self b])

(defn move
  [self]
  (def {:dir d} self)

  (update self :force * (- 1 (* 1.5 (get-frame-time))))
  (update self :force max 0)

  (let [{:force f} self]
    (update self :pos (fn [p]
                        (v/v+ p (v/v* d (* f (get-frame-time)))))))

  (update self :timer - (get-frame-time))

  (when (<= (self :timer) 0)
    (put self :dead true)
    (shoot :oil-spill :pos (self :pos)))

  self)

(defn oil-render
  [{:pos p :color c :size s :force f}]
  (let [half-size (v/v* s 0.5)]
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-translatef ;(v/v- p half-size) 0)
      (draw-rectangle-pro [0 0 ;s] half-size f c))))

(defn new-oil
  [pos dir force]
  @{:pos pos
    :dir dir
    :force (* (rng 1.8 2.1) force)
    :color :black
    :size [30 10]
    :timer 1
    :update move
    :render oil-render})

(defn flammable
  [self]
  (when (> (self :thickness) 0)
    (let [red (min (self :thickness) (* (self :thickness) 20 (get-frame-time)))]
      (update self :radius * (+ 1 red))
      (update self :thickness - red))))

(defn oil-spill-render
  [{:color c :radius r :pos p}]
  (draw-circle-v p r c))

(defn new-oil-spill
  [pos]
  @{:pos pos
    :color :black
    :radius (rng 20 35)
    :thickness 1
    :traction 0.1
    :update flammable
    :render oil-spill-render})

(defn shoot
  [kind &keys {:pos p}]
  (def {:bullets bs
        :player player
        :mp t} state)
  (def {:pos o} player)
  (def b (case kind
           :oil (new-oil o
                         (v/normalize (v/v- t o))
                         (v/dist o t))
           :oil-spill (new-oil-spill p)
           (error "no bullet")))

  (array/push bs b))

(defn small->zero
  [limit v]
  (if (and (> limit v)
           (< (- limit) v))
    0
    v))

(defn move-char
  [self]
  (update self :vel v/v* (+ (* 0.05 (- 1 (self :traction))) 0.95))
  (def opp-dir (map (fn [in v2]
                      (if (or (and (pos? in) (neg? in))
                              (and (pos? in) (neg? in)))
                        5

                        1))
                    (state :in)
                    (self :vel)))
  (draw-text opp-dir [10 10] :color :red)
  (draw-text (self :traction) [10 40] :color :red)
  (let [force
        (-> (v/normalize (state :in))
            (v/v*
              (* (self :speed)
                 (self :traction)
                 (get-frame-time)))
            #(v/v* opp-dir)
)

        stopper (-> [(if (zero? ((state :in) 0))
                       (small->zero 0.005 (* ((self :vel) 0) 0.1))
                       0)
                     (if (zero? ((state :in) 1))
                       (small->zero 0.005 (* ((self :vel) 1) 0.1))
                       0)]
                    (v/v*
                      (* -1
                         (self :speed)
                         (self :traction)
                         (get-frame-time))))

        force (v/v+ force stopper)

        target (v/v+ (self :vel) force)
        mag (v/mag target)]

    (if (> mag (self :max-speed))
      (put self :vel (v/v* (v/normalize target) (self :max-speed)))
      (put self :vel target))

    (update self :pos v/v+ (self :vel))))

(defn render-ui
  []
  (def {:player p
        :mp mp} state)
  (let [start (p :pos)
        end mp
        nof 10]
    (draw-circle-v start (p :radius) :red)
    (loop [i :range [1 (inc nof)]
           :let [v (/ i nof)
                 x (lerp (start 0) (end 0) v)
                 v (* v v)
                 y (lerp (start 1) (end 1) v)
                 p [x y] #(v-lerp start end v)
]]
      (draw-circle-v p 5 [1 0 0 0.7]))
    (draw-circle-v end 8 :red)))

(defn aim
  [self]
  (def {:mp mp
        :player player
        :bullets bs} self)

  (def {:pos origin} player)

  (move-char player)

  (loop [b :in bs]
    (:update b))

  (update self :bullets |(filter |(not ($ :dead)) $))

  (loop [b :in (self :bullets)]
    (:render b))

  (render-ui))
(defn render
  [el]
  (def {:player p
        :bullets bs} state)
  (update p :traction
          (fn [t]
            (var t 1)
            (loop [b :in bs
                   :let [{:traction nt} b]
                   :when nt
                   :when (circle-circle? b p)]
              (*= t nt))
            t))

  (aim state)

  (draw-text (state :in) [500 10] :color :white))

(defn fix-pos
  [[x y]]
  [(- x (dyn :offset-x))
   (- y (dyn :offset-y))])

(def down-dir
  {:w [[:in 1] dec]
   :s [[:in 1] inc]
   :a [[:in 0] dec]
   :d [[:in 0] inc]})

(def up-dir
  {:w [[:in 1] inc]
   :s [[:in 1] dec]
   :a [[:in 0] inc]
   :d [[:in 0] dec]})

(defn on-event
  [el ev]
  (match ev
    ([kind p] ({:press 1 :double-click 1} kind))
    (shoot (state :selected-bullet))

    ([kind p] ({:mouse-move 1 :drag 1} kind))
    (array-copy (state :mp) (fix-pos p))

    ([:key-down k] (down-dir k))
    (update-in state ;(down-dir k))

    ([:key-release k] (up-dir k))
    (update-in state ;(up-dir k))

    #(pp ev)
))

(start-game {:render render
             :on-event on-event
             :focus-on-load true})
