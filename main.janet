(use freja/flow)


### UTIL

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
              :speed 10
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
  [{:pos p :color c :size s}]
  (draw-rectangle-v (v/v- p (v/v* s 0.5)) s c))

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
    :update flammable
    :render oil-spill-render})

(defn shoot
  [kind &keys {:pos p}]
  (def {:bullets bs
        :origin o
        :mp t} state)
  (def b (case kind
           :oil (new-oil o
                         (v/normalize (v/v- t o))
                         (v/dist o t))
           :oil-spill (new-oil-spill p)
           (error "no bullet")))

  (array/push bs b))

(defn move-char
  [self]
  (update self :vel v/v* (* (self :traction) 0.95))
  (update self :vel
          (fn [v]
            (-> (v/normalize (state :in))
                (v/v*
                  (* (self :speed)
                     (self :traction)
                     (get-frame-time)))
                (v/v+ v))))
  (update self :pos v/v+ (self :vel)))

(defn aim
  [self]
  (def {:mp mp
        :player player
        :bullets bs} self)

  (def {:pos origin} player)

  (move-char player)

  (let [start origin
        end mp
        nof 10]
    (draw-circle-v start 10 :red)
    (loop [i :range [0 nof]
           :let [v (/ i nof)
                 x (lerp (start 0) (end 0) v)
                 v (* v v)
                 y (lerp (start 1) (end 1) v)
                 p [x y] #(v-lerp start end v)
]]
      (draw-circle-v p 10 :red))
    (draw-circle-v end 10 :red))

  (loop [b :in bs]
    (:update b))

  (update self :bullets |(filter |(not ($ :dead)) $))

  (loop [b :in (self :bullets)]
    (:render b)))

(defn render
  [el]
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
