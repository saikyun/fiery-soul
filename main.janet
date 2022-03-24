#(use freja/flow)
(use freja-jaylib)
(import freja/vector-math :as v)


### UTIL

(defn small->zero
  [limit v]
  (if (and (> limit v)
           (< (- limit) v))
    0
    v))

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

(defn v-lerp4
  [[x y z w] [x2 y2 z2 w2] v]
  [(lerp x x2 v)
   (lerp y y2 v)
   (lerp z z2 v)
   (lerp w w2 v)])

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
              :vel @[0 0]
              :in @[0 0]}
    :bullets @[]
    :selected-bullet :oil
    :in @[0 0]})

(varfn shoot [& args])

(defn oil-move
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
    :radius 15
    :timer 1
    :update oil-move
    :render oil-render})

(defn clamp
  [bottom top v]
  (min top (max bottom v)))

(defn flammable
  [self]
  (when (> (self :thickness) 0)
    (let [red (min (self :thickness) (* (self :thickness) 20 (get-frame-time)))]
      (update self :radius * (+ 1 red))
      (update self :thickness - red)))

  (loop [b :in (state :bullets)
         :when (circle-circle? b self)
         :when (b :heat)
         :when (not= b self)]
    (if (>= (self :heat) 100)
      (do
        (update b :heat + (* 0.1 (self :heat)))
        (put self :heat 100))
      (do
        (update b :heat + (* 0.1 (self :heat)))
        (update self :heat - (* 0.09 (self :heat)))))))

(defn oil-spill-render
  [self]
  (def {:color c
        :radius r
        :pos p
        :heat heat
        :no-burn nb} self)
  (if (and (>= heat 100) (not nb))
    (do (put self :no-burn true)
      (draw-circle-v p r :yellow)
      (ev/sleep 0.05))
    (if (>= heat 100)
      (draw-circle-v p r [(clamp 0 0.7 (/ heat 100)) 0 0 1])
      (draw-circle-v p r [(clamp 0 0.7 (/ heat 300)) 0 0 1]))))

(defn new-oil-spill
  [pos]
  @{:pos pos
    :color :black
    :radius (rng 20 35)
    :thickness 1
    :traction 0.1
    :heat 0
    :update flammable
    :render oil-spill-render})

(defn fire-render
  [{:pos p :color c :size s :force f :dir dir}]
  (let [half-size (v/v* s 0.5)]
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-translatef ;(v/v- p half-size) 0)
      (draw-rectangle-pro [0 0 ;s]
                          half-size
                          (+ 90 (* (math/atan2 ;dir) -180 (/ 1 math/pi))) c))))

(defn fire-move
  [self]
  (def {:dir d} self)

  (update self :force * (- 1 (* 3 (get-frame-time))))
  (update self :force max 0)

  (let [{:force f} self]
    (update self :pos (fn [p]
                        (v/v+ p (v/v* d (* f (get-frame-time)))))))

  (update self :timer - (get-frame-time))

  (when (<= (self :timer) 0)
    (put self :dead true))

  (unless (self :dead)
    (loop [b :in (state :bullets)
           :when (circle-circle? b self)
           :when (b :heat)
           :when (not= b self)]
      (update b :heat + (self :heat))))

  self)

(defn new-fire
  [pos dir]
  @{:pos pos
    :dir dir
    :force (* (rng 1.8 2.1) 1000)
    :color :red
    :size [30 10]
    :radius 15
    :heat 10
    :timer 1
    :update fire-move
    :render fire-render})

(defn move-char
  [self]
  (update self :vel v/v* (+ (* 0.05 (- 1 (self :traction))) 0.95))
  (def opp-dir (map (fn [in v2]
                      (if (or (and (pos? in) (neg? in))
                              (and (pos? in) (neg? in)))
                        5

                        1))
                    (self :in)
                    (self :vel)))

  (let [force
        (-> (v/normalize (self :in))
            (v/v*
              (* (self :speed)
                 (self :traction)
                 (get-frame-time)))
            #(v/v* opp-dir)
)

        stopper (-> [(if (zero? ((self :in) 0))
                       (small->zero 0.005 (* ((self :vel) 0) 0.1))
                       0)
                     (if (zero? ((self :in) 1))
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


(def damage-timer 0.05)

(defn enemy-render
  [self]
  (def {:pos p
        :color c
        :radius r
        :dir dir
        :heat heat
        :damage-timer dmg-timer} self)

  (if dmg-timer
    (do
      (update self :damage-timer - (get-frame-time))
      (update self :damage-timer (fn [v] (if (<= v 0) nil v)))

      (draw-circle-v (v/v+ p
                           [(rng -10 10)
                            (rng -10 10)])
                     r
                     (v-lerp4 c [1 0 0 1]
                              (clamp 0 1 (/ heat 100)))))

    (draw-circle-v p r (v-lerp4 c [1 0 0 1]
                                (clamp 0 1 (/ heat 100))))))

(defn target-player
  [self]
  (let [dir (v/v- (get-in state [:player :pos])
                  (self :pos))]
    (put self :in (v/normalize dir)))

  (when (circle-circle? self (state :player))
    (put-in state [:player :dead] true)))

(defn damage
  [o dmg]
  (update o :hp - dmg)
  (put o :damage-timer damage-timer)
  (when (<= (o :hp) 0)
    (put o :dead true)))

(def burn-timer 0.1)

(defn check-burn
  [self]
  (def {:heat h} self)
  (def burning? (>= h 100))
  (var burned nil)
  (if (not burning?)
    (put self :burn-timer 0)
    (do
      (update self :burn-timer
              (fn [v]
                (if (nil? v)
                  (do
                    (set burned true)
                    0)
                  (let [new-v (- v (get-frame-time))]
                    (if (<= new-v 0)
                      (do (set burned true)
                        burn-timer)
                      new-v)))))))

  (when burned
    (damage self 5)))

(defn new-enemy
  [&keys {:pos pos}]
  (default pos [(rng 400 700) (rng 400 700)])

  @{:pos pos
    :color [(rng 0 1) (rng 0 1) (rng 0 1) 0.9]
    :vel @[0 0]
    :speed 10
    :max-speed 10
    :traction 1
    :in @[0 0]
    :radius 40
    :hp 200
    :heat 0
    :update (fn [self]
              (target-player self)
              (move-char self)
              (check-burn self))
    :render enemy-render})

(varfn shoot
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
           :fire (new-fire o (v/normalize (v/v- t o)))
           :enemy (new-enemy :pos p)
           (error "no bullet")))

  (array/push bs b))

(defn render-ui
  [rs]
  (def {:player p
        :mp mp
        :selected-bullet selected-bullet
        :in in} state)

  (when (p :dead)
    (os/exit 1)
    (draw-text "SOUL BURN"
               (v/v* rs (rng 0.499 0.501))
               :center true
               :color :red))

  (array-copy (p :in) in)

  (when (> 2 (rng 0 150))
    (shoot :enemy :pos [(rng 0 700) (rng 0 700)]))

  (let [start (p :pos)
        end mp
        nof 10]
    (draw-circle-v start (p :radius) :red)
    (case selected-bullet
      :oil
      (loop [i :range [1 (inc nof)]
             :let [v (/ i nof)
                   x (lerp (start 0) (end 0) v)
                   v (* v v)
                   y (lerp (start 1) (end 1) v)
                   p [x y] #(v-lerp start end v)
]]
        (draw-circle-v p 5 [1 0 0 0.7]))

      :fire
      (loop [i :range [1 (inc nof)]
             :let [v (/ i nof)
                   x (lerp (start 0) (end 0) v)
                   #               v (* v v)
                   y (lerp (start 1) (end 1) v)
                   p [x y] #(v-lerp start end v)
]]
        (draw-circle-v p 5 [0.1 0.1 0.1 0.7])))
    (draw-circle-v end 8 :red)))

(defn aim
  [self]
  (def {:mp mp
        :player player
        :bullets bs} self)

  (def {:pos origin} player)

  (unless (player :dead)
    (move-char player))

  (loop [b :in bs]
    (:update b))

  (update self :bullets |(filter |(not ($ :dead)) $))

  (loop [b :in (self :bullets)]
    (:render b)))

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

  (render-ui [(el :width) (el :height)])

  #(draw-text (state :in) [500 10] :color :white)
)


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

(def bullet-kind
  {:1 [[:selected-bullet] :oil]
   :2 [[:selected-bullet] :fire]})

(def ks [:1 :2 :w :s :a :d])

(defn on-event
  [el ev]
  (match ev
    ([kind p] ({:press 1 :double-click 1} kind))
    (unless ((state :player) :dead)
      (shoot (state :selected-bullet)))

    ([kind p] ({:mouse-move 1 :drag 1} kind))
    (unless ((state :player) :dead)
      (array-copy (state :mp) (fix-pos p)))

    ([:key-down k] (down-dir k))
    (update-in state ;(down-dir k))

    ([:key-release k] (up-dir k))
    (update-in state ;(up-dir k))

    ([:key-release k] (bullet-kind k))
    (put-in state ;(bullet-kind k))

    #(pp ev)
))


(defn init
  []
  (shoot :enemy :pos [(rng 400 700) (rng 400 700)]))

(when (dyn :freja/loading-file)
  (init)

  '(start-game {:render render
               :on-event on-event
               :focus-on-load true}))

(defn main
  [& _]
  (init-window 0 0 "Fiery Soul")

  (toggle-fullscreen)

  (init)

  (set-target-fps 60)

  (var last-mp nil)

  (with-dyns [:offset-x 0 :offset-y 0]
    (while (not (window-should-close))
      (begin-drawing)
      (clear-background :white)

      (def el {:width (get-screen-width)
               :height (get-screen-height)
               :render-x 0
               :render-y 0
               :focused? true})

      (let [new-mp (get-mouse-position)]
        (unless (= new-mp last-mp)
          (set last-mp new-mp)
          (on-event el [:mouse-move new-mp])))

      (loop [k :in ks]
        (when (key-pressed? k) (on-event el [:key-down k]))
        (when (key-released? k) (on-event el [:key-release k])))

      (loop [mb :in [0]]
        (when (mouse-button-pressed? mb) (on-event el [:press last-mp])))

      (render el)
      (end-drawing)))

  (close-window))
