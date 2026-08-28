(ns voxel.world
  "Pure game simulation: voxel structure, cannonball ballistics, blast
  destruction, structural stability, falling chunks, scoring, win/lose.

  No raylib here — everything is plain data + math so it is fully testable.

  Scoring uses the divergence-theorem mesh volume (voxel.mesh): the remaining
  structure volume is measured from its extracted surface mesh, and each
  detached chunk's volume/centroid are measured the same way at detach time."
  (:require [voxel.mesh :as mesh]))

(def GRAVITY 25.0)
(def BALL-RADIUS 0.5)
(def BALL-MIN-SPEED 18.0)
(def BALL-MAX-SPEED 42.0)
(def BALLS-PER-ROUND 5)
(def BLAST-RADIUS 2.6)
(def SHATTER-SPEED 8.0)
(def WIN-THRESHOLD 0.7)
(def SETTLE-FRAMES-TO-LOSE 45)
(def MUZZLE [0.0 3.0 22.0])

;; --- aiming ------------------------------------------------------------

(defn dir-from-yaw-pitch
  "Unit direction vector for yaw (0 = straight down -z, + = to +x) and pitch
  (0 = level, + = up)."
  [yaw pitch]
  [(* (Math/cos pitch) (Math/sin yaw))
   (Math/sin pitch)
   (- (* (Math/cos pitch) (Math/cos yaw)))])

(defn trajectory-points
  "Ballistic sample points from pos with velocity v (step dt) until the arc
  reaches y <= 0, for the aiming preview. Capped at 400 points."
  [pos v dt]
  (loop [x (pos 0) y (pos 1) z (pos 2)
         vx (v 0) vy (v 1) vz (v 2)
         pts []]
    (let [vy' (- vy (* GRAVITY dt))
          x'  (+ x (* vx dt))
          y'  (+ y (* vy' dt))
          z'  (+ z (* vz dt))
          pts' (conj pts [x y z])]
      (if (or (<= y' 0.0) (> (count pts') 400))
        (conj pts' [x' y' z'])
        (recur x' y' z' vx vy' vz pts')))))

;; --- structure / connectivity ------------------------------------------

(defn components
  "6-connected components of a voxel map, as a seq of sets of cells."
  [voxels]
  (let [seen (volatile! #{})
        dirs [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]]
    (for [cell (keys voxels)
          :when (not (contains? @seen cell))
          :let [comp (loop [frontier [cell] acc #{}]
                       (if (empty? frontier)
                         acc
                         (let [c (peek frontier)
                               acc' (conj acc c)]
                           (vswap! seen conj c)
                           (recur (into (pop frontier)
                                        (filter #(and (contains? voxels %)
                                                      (not (contains? acc' %)))
                                                (map #(mapv + c %) dirs)))
                                  acc'))))]]
      comp)))

(defn grounded-cells
  "The set of cells reachable from the ground plane (j = 0) through filled
  cells — the anchored part of the structure."
  [voxels]
  (let [start (filter #(zero? (second %)) (keys voxels))
        dirs [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]]
    (loop [frontier (vec start) acc (set start)]
      (if (empty? frontier)
        acc
        (let [c (peek frontier)
              nbrs (filter #(and (contains? voxels %) (not (contains? acc %)))
                           (map #(mapv + c %) dirs))]
          (recur (into (pop frontier) nbrs) (into acc nbrs)))))))

;; --- the castle level ----------------------------------------------------

(defn- ring? [di dk] (or (zero? di) (= di 2) (zero? dk) (= dk 2)))

(defn castle
  "Two hollow towers with crenellations, joined by a crenellated curtain wall
  with a gate arch. ~220 voxels around k = -22, j from 0."
  []
  (let [oz -23
        tower (fn [ox]
                (into {}
                      (for [di (range 3) dj (range 10) dk (range 3)
                            :when (or (zero? dj) (ring? di dk))]
                        [[(+ ox di) dj (+ oz dk)] (if (zero? dj) :stone-dark :stone)])))
        crenel (fn [ox]
                 (into {}
                       (for [di (range 3) dk (range 3)
                             :when (and (ring? di dk) (even? (+ di dk)))]
                         [[(+ ox di) 10 (+ oz dk)] :gold])))
        wall (into {}
                   (for [i (range -4 5) dj (range 6)
                         :when (not (and (< dj 2) (< (Math/abs i) 2)))]
                     [[i dj -22] :stone]))
        wall-crenel (into {}
                          (for [i (range -4 5) :when (even? i)]
                            [[i 6 -22] :gold]))]
    (merge (tower -7) (tower 5) (crenel -7) (crenel 5) wall wall-crenel)))

(defn initial-state
  []
  (let [vox (castle)]
    {:phase :playing
     :voxels vox
     :chunks []
     :ball nil
     :balls-left BALLS-PER-ROUND
     :muzzle MUZZLE
     :blast-radius BLAST-RADIUS
     :initial-volume (mesh/mesh-volume (mesh/surface-triangles vox))
     :events []
     :destruction 0.0
     :settle-frames 0}))

;; --- scoring -------------------------------------------------------------

(defn destruction-fraction
  "1 - (remaining volume / initial volume), where remaining volume is measured
  with the divergence-theorem mesh volume (static surface + chunk volumes)."
  [state]
  (let [v0 (:initial-volume state)]
    (if (zero? v0)
      0.0
      (let [remaining (+ (mesh/mesh-volume (mesh/surface-triangles (:voxels state)))
                         (reduce + (map #(or (:volume %) (count (:cells %)))
                                        (:chunks state))))]
        (- 1.0 (/ remaining v0))))))

;; --- stability ------------------------------------------------------------

(defn- stabilize
  "Detach every floating (ungrounded) component into falling chunks. Each
  chunk's volume and centroid are measured from its surface mesh."
  [state]
  (let [vox (:voxels state)
        anchored (grounded-cells vox)
        floating (components (select-keys vox (filter #(not (contains? anchored %)) (keys vox))))
        to-chunk (fn [cells]
                   (let [cmap (select-keys vox cells)]
                     {:cells cmap
                      :volume (mesh/mesh-volume (mesh/surface-triangles cmap))
                      :centroid (mesh/mesh-centroid (mesh/surface-triangles cmap))
                      :off 0.0
                      :vy 0.0}))]
    (let [state' (assoc state
                        :voxels (select-keys vox anchored)
                        :chunks (into (:chunks state) (map to-chunk floating)))]
      ;; destruction-fraction re-extracts the whole surface mesh (~ms-scale);
      ;; recompute it only here, where the structure has actually changed, so
      ;; the per-frame phase/HUD reads stay cheap
      (assoc state' :destruction (destruction-fraction state')))))

;; --- blast ----------------------------------------------------------------

(defn blast-at
  "Destroy every voxel whose cell centre is within `radius` of world point p,
  then restabilize the structure and check the win condition."
  [state p radius]
  (let [[px py pz] p
        vox (:voxels state)
        r2 (* radius radius)
        near? (fn [cell]
                (let [[i j k] cell
                      dx (- (+ i 0.5) px)
                      dy (- (+ j 0.5) py)
                      dz (- (+ k 0.5) pz)]
                  (<= (+ (* dx dx) (* dy dy) (* dz dz)) r2)))
        doomed (filter near? (keys vox))
        vox' (reduce dissoc vox doomed)
        state' (stabilize (assoc state :voxels vox'))
        state' (if (seq doomed)
                 (update state' :events conj (into [:blast] (conj (vec p) (count doomed))))
                 state')]
    (if (and (= :playing (:phase state'))
             (>= (destruction-fraction state') WIN-THRESHOLD))
      (assoc state' :phase :won)
      state')))

;; --- cannonball -------------------------------------------------------------

(defn fire
  "Launch a ball from the muzzle along unit `dir` with power in [0,1].
  No-op unless playing, ball-free and with ammo left."
  [state dir power]
  (if (or (not= :playing (:phase state))
          (some? (:ball state))
          (zero? (:balls-left state)))
    state
    (let [len (Math/sqrt (reduce + (map #(* % %) dir)))
          [dx dy dz] (map #(/ % len) dir)
          speed (+ BALL-MIN-SPEED (* (- BALL-MAX-SPEED BALL-MIN-SPEED) power))
          [mx my mz] (:muzzle state)]
      (-> state
          (assoc :ball {:x mx :y my :z mz
                        :vx (* dx speed) :vy (* dy speed) :vz (* dz speed)})
          (update :balls-left dec)))))

(defn- cell-at
  "The voxel cell containing world point [x y z], if any."
  [voxels x y z]
  (let [cell [(int (Math/floor x)) (int (Math/floor y)) (int (Math/floor z))]]
    (when (contains? voxels cell)
      cell)))

(defn- ball-substeps
  "Advance the ball by dt in 4 substeps. Returns the moved ball map, the vector
  [:blast x y z] on a voxel hit, or nil when the ball dies quietly (ground or
  out of bounds)."
  [b voxels dt]
  (let [h (/ dt 4.0)]
    (loop [b b i 0]
      (if (= i 4)
        b
        (let [{:keys [x y z vx vy vz]} b
              vy' (- vy (* GRAVITY h))
              x' (+ x (* vx h))
              y' (+ y (* vy' h))
              z' (+ z (* vz h))
              b' (assoc b :x x' :y y' :z z' :vy vy')]
          (cond
            (or (< y' BALL-RADIUS)
                (> y' 300.0)
                (> (Math/abs x') 80.0)
                (> z' 40.0)
                (< z' -80.0)) nil
            (cell-at voxels x' y' z') [:blast x' y' z']
            :else (recur b' (inc i))))))))

(defn- update-ball
  [state dt]
  (if-let [b (:ball state)]
    (let [r (ball-substeps b (:voxels state) dt)]
      (cond
        (nil? r) (assoc state :ball nil)
        (vector? r) (-> state
                        (assoc :ball nil)
                        (blast-at (rest r) (:blast-radius state)))
        :else (assoc state :ball r)))
    state))

;; --- falling chunks ----------------------------------------------------------

(defn- column-top
  "Highest static-voxel j below `j` in column [i _ k], or -1 for bare ground."
  [voxels i j k]
  (or (some (fn [jj] (when (contains? voxels [i jj k]) jj))
            (range (dec j) -1 -1))
      -1))

(defn- chunk-land-offset
  "How far (in cell units) `cells` can drop before the lowest of them rests on
  the static grid or the ground."
  [cells voxels]
  (apply min
         (for [[i j k] (keys cells)]
           (- j (column-top voxels i j k) 1))))

(defn- chunk-centre
  "Rough world-space centre of a falling chunk (mean of cell centres, offset by
  its drop distance)."
  [ch]
  (let [cells (keys (:cells ch))
        n (count cells)
        mean (fn [f] (/ (reduce + (map f cells)) n))]
    [(mean first) (- (mean second) (:off ch)) (mean #(nth % 2))]))

(defn- update-chunks
  [state dt]
  (let [vox (volatile! (:voxels state))
        kept (volatile! [])
        events (volatile! [])
        landed? (volatile! false)]
    (doseq [ch (:chunks state)]
      (let [vy'  (- (:vy ch) (* GRAVITY dt))
            ;; :off is a positive drop distance (cell world y = j - off), and vy
            ;; is negative while falling, so d(off)/dt = -vy.
            off' (+ (:off ch) (* (- vy') dt))
            land (chunk-land-offset (:cells ch) @vox)]
        (if (< off' land)
          (vswap! kept conj (assoc ch :vy vy' :off off'))
          (do
            (vreset! landed? true)
            (if (> (Math/abs vy') SHATTER-SPEED)
              (vswap! events conj (into [:shatter] (conj (vec (chunk-centre ch))
                                                          (count (:cells ch)))))
              (vswap! vox into (into {} (for [[[i j k] m] (:cells ch)]
                                          [[i (- j land) k] m]))))))))
    (let [state' (assoc state
                        :voxels @vox
                        :chunks @kept
                        :events (into (:events state) @events))]
      (if @landed?
        (stabilize state')
        state'))))

;; --- phase -------------------------------------------------------------------

(defn- update-phase
  [state]
  (if (not= :playing (:phase state))
    state
    (let [quiet? (and (zero? (:balls-left state))
                      (nil? (:ball state))
                      (empty? (:chunks state)))]
      (cond
        (>= (:destruction state) WIN-THRESHOLD)
        (assoc state :phase :won)

        quiet?
        (let [n (inc (or (:settle-frames state) 0))]
          (if (>= n SETTLE-FRAMES-TO-LOSE)
            (assoc state :phase :lost :settle-frames n)
            (assoc state :settle-frames n)))

        :else (assoc state :settle-frames 0)))))

(defn step
  "Advance the world by dt seconds: ball flight, chunk falls, phase checks."
  [state dt]
  (-> state
      (update-ball dt)
      (update-chunks dt)
      (update-phase)))
