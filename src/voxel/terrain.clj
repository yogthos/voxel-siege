(ns voxel.terrain
  "Pure scenery layout for the battlefield: low hills and pine trees
  scattered around the firing corridor. Placement is deterministic from a
  seed (own LCG — no native calls), so a given seed always dresses the same
  scene and the module stays headless-testable.")

(def FIELD-MIN-X -64.0)
(def FIELD-MAX-X 64.0)
(def FIELD-MIN-Z -64.0)
(def FIELD-MAX-Z 40.0)

;; the firing corridor: muzzle (z 22) downrange past the castle (z -23).
;; Scenery must keep its skirts this far from the rectangle.
(def CORRIDOR-MIN-X -11.0)
(def CORRIDOR-MAX-X 11.0)
(def CORRIDOR-MIN-Z -30.0)
(def CORRIDOR-MAX-Z 28.0)

(def HILL-COUNT 9)
(def TREE-COUNT 22)

;; --- deterministic LCG (Numerical Recipes constants) -------------------------

(defn- lcg
  [state]
  (let [s (mod (+ (* 1664525 (long state)) 1013904223) 2147483647)]
    [s (/ (double s) 2147483647.0)]))

(defn- rand-range
  "state lo hi -> [state' value in [lo hi)]."
  [state lo hi]
  (let [[s u] (lcg state)]
    [s (+ lo (* u (- hi lo)))]))

(defn corridor-distance
  "Distance from a point to the keep-clear corridor rectangle (0 = inside)."
  [x z]
  (let [dx (max (- CORRIDOR-MIN-X x) (- x CORRIDOR-MAX-X) 0.0)
        dz (max (- CORRIDOR-MIN-Z z) (- z CORRIDOR-MAX-Z) 0.0)]
    (Math/sqrt (+ (* dx dx) (* dz dz)))))

(defn- spread
  [a b]
  (Math/sqrt (+ (* (- (:x a) (:x b)) (- (:x a) (:x b)))
                (* (- (:z a) (:z b)) (- (:z a) (:z b))))))

(defn- make-hill
  [state]
  (let [[s1 x] (rand-range state FIELD-MIN-X FIELD-MAX-X)
        [s2 z] (rand-range s1 FIELD-MIN-Z FIELD-MAX-Z)
        [s3 r] (rand-range s2 5.0 9.0)
        [s4 h] (rand-range s3 1.6 3.2)]
    [s4 {:x x :z z :base-r r :height h}]))

(defn- hill-ok?
  [h placed]
  (and (> (corridor-distance (:x h) (:z h)) (+ (:base-r h) 2.0))
       (every? #(> (spread h %) (+ (:base-r h) (:base-r %) 3.0)) placed)))

(defn- make-tree
  [state]
  (let [[s1 x] (rand-range state FIELD-MIN-X FIELD-MAX-X)
        [s2 z] (rand-range s1 FIELD-MIN-Z FIELD-MAX-Z)
        [s3 s] (rand-range s2 0.8 1.5)]
    [s3 {:x x :z z :scale s}]))

(defn- tree-ok?
  [t hills placed]
  (and (> (corridor-distance (:x t) (:z t)) 3.0)
       (every? #(> (spread t %) (+ (:base-r %) 1.5)) hills)
       (every? #(> (spread t %) 3.5) placed)))

(defn- place
  "Take up to `want` candidates from `make` (max `tries` draws), keeping the
  ones `ok?` accepts. -> [state' kept]"
  [state want tries make ok?]
  (loop [s state kept [] n 0 tries-left tries]
    (if (or (== n want) (zero? tries-left))
      [s kept]
      (let [[s' cand] (make s)]
        (if (ok? cand kept)
          (recur s' (conj kept cand) (inc n) (dec tries-left))
          (recur s' kept n (dec tries-left)))))))

(defn scene
  "Static scenery for a round: {:hills (...) :trees (...)}. Deterministic
  per seed."
  ([] (scene 20260917))
  ([seed]
   (let [[s1 hills] (place seed HILL-COUNT 90 make-hill hill-ok?)
         [_ trees] (place s1 TREE-COUNT 200 make-tree
                          (fn [c kept] (tree-ok? c hills kept)))]
     {:hills hills :trees trees})))
