(ns voxel.world
  "Pure game simulation. The castle is rigid bodies from frame one — there is
  no static voxel map, no freeze-back-into-grid, and no detach-on-blast.
  Blasts destroy cells and split bodies along the rubble; voxel.physics owns
  the real Box3D motion and reports plain-data facts that `apply-physics`
  folds back in. Everything here is plain data + math, fully testable
  headlessly.

  Volume accounting is cell count: every body is a set of unit cubes, so a
  body's volume is exactly its cell count however it is rotated, and the
  destruction fraction is destroyed-cells / initial-cells."
  )

(def GRAVITY 25.0)
(def BALL-RADIUS 0.5)
(def BALL-MIN-SPEED 18.0)
(def BALL-MAX-SPEED 42.0)
(def BALLS-PER-ROUND 5)
(def BLAST-RADIUS 2.6)
(def SHATTER-SPEED 18.0)
(def SETTLE-SPEED 0.15)
(def SETTLE-SLOW-FRAMES 30)
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
  reaches y <= 0, for the aiming preview. Capped at 400 points. The real ball
  flies in Box3D (voxel.physics); this only approximates the first arc."
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

;; --- connectivity --------------------------------------------------------

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

;; --- the castle level ----------------------------------------------------

(defn- ring? [di dk] (or (zero? di) (= di 2) (zero? dk) (= dk 2)))

(defn castle-parts
  "The level as three disjoint parts — west tower (+crenels), curtain wall
  (+crenels), east tower (+crenels). Each part becomes one rigid body spawned
  asleep, so the standing castle costs nothing until disturbed but topples
  for real once its support is blown out."
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
    [(merge (tower -7) (crenel -7))
     (merge wall wall-crenel)
     (merge (tower 5) (crenel 5))]))

(defn- make-body
  "A body record: cells relative to :anchor, :pos/:quat its live pose (equal
  to the anchor/identity until physics reports otherwise)."
  [id cells asleep]
  (let [ks (keys cells)
        cnt (count ks)
        mean (fn [f] (/ (reduce + (map f ks)) cnt))
        anchor [(double (+ (mean first) 0.5))
                (double (+ (mean second) 0.5))
                (double (+ (mean #(nth % 2)) 0.5))]]
    {:id id
     :cells cells
     :anchor anchor
     :body nil
     :pos anchor
     :quat [0.0 0.0 0.0 1.0]
     :speed 0.0
     :asleep asleep}))

(defn initial-state
  []
  (let [parts (castle-parts)
        bodies (map-indexed (fn [i cells] (make-body (inc i) cells true)) parts)]
    {:phase :playing
     :bodies (vec bodies)
     :body-seq (inc (count bodies))
     :ball nil
     :balls-left BALLS-PER-ROUND
     :muzzle MUZZLE
     :blast-radius BLAST-RADIUS
      :initial-volume (reduce + (map (comp count :cells) bodies))
      :destroyed-cells 0
      :events []
     :destruction 0.0
     :settle-frames 0}))

;; --- scoring -------------------------------------------------------------

(defn destruction-fraction
  "Destroyed cells / initial cells. A cell counts as destroyed exactly once:
  when a blast carves it, or when its body first breaks into rubble. Bodies
  are unit cubes, so cell count is volume regardless of rotation."
  [state]
  (let [v0 (:initial-volume state)]
    (if (zero? v0)
      0.0
      (min 1.0 (/ (double (or (:destroyed-cells state) 0)) v0)))))

;; --- blast ----------------------------------------------------------------

(defn- split-into-bodies
  "Re-body a hit body's surviving cells: one new body per 6-connected group.
  Children inherit the parent's anchor and pose — world cell positions are
  unchanged — and are awake with :body nil so voxel.physics spawns them
  fresh (the old body's shapes cannot be removed in place)."
  [parent remaining groups next-id]
  (vec (map-indexed (fn [i group]
                       {:id (+ next-id i)
                        :cells (select-keys remaining group)
                        :anchor (:anchor parent)
                        :body nil
                        :pos (:pos parent)
                        :quat (:quat parent)
                        :vel (:vel parent)
                        :speed 0.0
                        :asleep false
                        :rubble (:rubble parent)})
                     groups)))

(defn blast-at
  "Destroy every cell within `radius` of world point p across all bodies,
  splitting hit bodies into their connected remains. Unhit bodies are passed
  through untouched (keeping their physics body)."
  [state p radius]
  (let [[px py pz] p
        r2 (* radius radius)
        near? (fn [cell]
                (let [[i j k] cell
                      dx (- (+ i 0.5) px)
                      dy (- (+ j 0.5) py)
                      dz (- (+ k 0.5) pz)]
                  (<= (+ (* dx dx) (* dy dy) (* dz dz)) r2)))
        step (fn [st body]
               (let [doomed (filter near? (keys (:cells body)))
                     n (count doomed)]
                 (if (zero? n)
                   (update st :bodies conj body)
                   (let [remaining (apply dissoc (:cells body) doomed)
                         groups (components remaining)
                         new-bodies (split-into-bodies body remaining groups (:body-seq st))]
                     (-> st
                         (update :bodies into new-bodies)
                         (assoc :body-seq (+ (:body-seq st) (count new-bodies)))
                          (update :hit-cells + n)
                          ;; rubble cells were counted destroyed when they
                          ;; first broke - carving them again is not more
                          ;; destruction
                          (update :destroyed-cells + (if (:rubble body) 0 n)))))))
        seed (assoc state :bodies [] :hit-cells 0)
        st (reduce step seed (:bodies state))
        n (:hit-cells st)
        st (dissoc st :hit-cells)
        st (if (pos? n)
             (update st :events conj (into [:blast] (conj (vec p) n)))
             st)
        st (assoc st :destruction (destruction-fraction st))]
    (if (and (= :playing (:phase st))
             (>= (:destruction st) WIN-THRESHOLD))
      (assoc st :phase :won)
      st)))

;; --- cannonball -------------------------------------------------------------

(defn fire
  "Launch a ball from the muzzle along unit `dir` with power in [0,1].
  Returns the ball record with its launch velocity; voxel.physics spawns the
  Box3D body. No-op unless playing, ball-free and with ammo left."
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
          (assoc :ball {:origin [mx my mz]
                        :v [(* dx speed) (* dy speed) (* dz speed)]
                        :body nil
                        :pos nil})
          (update :balls-left dec)))))

(defn ball-impact-cell
  "The first occupied cell the sphere at pos with radius r overlaps, or nil.
  A cell overlaps when the closest point of its unit cube to pos is within r."
  [voxels pos r]
  (let [[x y z] pos
        lo [(int (Math/floor (- x r))) (int (Math/floor (- y r))) (int (Math/floor (- z r)))]
        hi [(int (Math/floor (+ x r))) (int (Math/floor (+ y r))) (int (Math/floor (+ z r)))]
        r2 (* r r)
        overlap? (fn [i j k]
                   (let [cx (max i (min x (+ i 1.0)))
                         cy (max j (min y (+ j 1.0)))
                         cz (max k (min z (+ k 1.0)))
                         dx (- x cx) dy (- y cy) dz (- z cz)]
                     (<= (+ (* dx dx) (* dy dy) (* dz dz)) r2)))]
    (first (for [i (range (lo 0) (inc (hi 0)))
                 j (range (lo 1) (inc (hi 1)))
                 k (range (lo 2) (inc (hi 2)))
                 :when (and (contains? voxels [i j k]) (overlap? i j k))]
             [i j k]))))

;; --- physics facts ---------------------------------------------------------

(defn attach-bodies
  "Attach physics body ids to pending world bodies (keyed by world :id) and
  to the ball, after voxel.physics spawned them."
  [state {:keys [bodies ball]}]
  (let [id->body bodies]
    (cond-> state
      (seq id->body)
      (update :bodies (fn [bs]
                        (mapv #(if-let [b (get id->body (:id %))]
                                 (assoc % :body b)
                                 %)
                              bs)))

      (and (some? ball) (some? (:ball state)))
      (assoc-in [:ball :body] ball))))

(defn live-body-ids
  "Every Box3D body id still referenced by the world (the ball's and one per
  attached body). voxel.physics destroys the rest."
  [state]
  (let [ball-body (get-in state [:ball :body])]
    (cond-> (into #{} (keep :body (:bodies state)))
      (some? ball-body) (conj ball-body))))

(defn- check-win
  [state]
  (if (and (= :playing (:phase state))
           (>= (:destruction state) WIN-THRESHOLD))
    (assoc state :phase :won)
    state))

(defn all-cells
  "Union of every body's cells — the occupied-world map for ball collision."
  [state]
  (into {} (map :cells (:bodies state))))

(defn- apply-ball-fact
  "Fold the ball's physics fact into the world: impact blasts the structure,
  ground/out-of-bounds/sleep despawns it, otherwise the position updates."
  [state fact]
  (cond
    (nil? fact) (assoc state :ball nil)
    (nil? (:ball state)) state
    :else
    (let [{:keys [pos asleep]} fact
          [x y z] pos
          hit (ball-impact-cell (all-cells state) pos BALL-RADIUS)]
      (cond
        hit (-> state
                (assoc :ball nil)
                (blast-at pos (:blast-radius state)))

        (or asleep
            (< y BALL-RADIUS) (> y 300.0)
            (> (Math/abs x) 80.0) (> z 40.0) (< z -80.0))
        (assoc state :ball nil)

        :else (let [slow (if (< (:speed fact) SETTLE-SPEED)
                           (inc (or (get-in state [:ball :slow]) 0))
                           0)]
                (if (>= slow SETTLE-SLOW-FRAMES)
                  (assoc state :ball nil)
                  (-> state
                      (assoc-in [:ball :pos] pos)
                      (assoc-in [:ball :slow] slow))))))))

(defn- shatter-into
  "Replace a body that hit too hard with one persistent rubble body per
  cell at the impact pose: fallen blocks stay on the scene instead of
  vanishing. Fragments inherit the impact velocity so the rubble sprays."
  [body fact next-id]
  (mapv (fn [i cell]
          {:id (+ next-id i)
           :cells {cell (get (:cells body) cell)}
           :anchor (:anchor body)
           :body nil
           :pos (:pos fact)
           :quat (:quat fact)
           :vel (:vel fact)
           :speed 0.0
           :asleep false
           :rubble true})
        (range)
        (keys (:cells body))))

(defn- apply-body-facts
  "Fold one frame of body facts into the world. A sudden speed drop past
  SHATTER-SPEED breaks the body into per-cell rubble that persists (blocks
  that fall down stay on the scene); a body resting below the ground plane
  is a physics blow-through and is cleared. Otherwise the reported
  transform is recorded — resting bodies simply stay bodies; nothing is
  ever frozen back into a grid."
  [state facts]
  (if (empty? facts)
    state
    (let [by-body (into {} (map (juxt :body identity)) facts)
          kept (volatile! [])
          frags (volatile! [])
          events (volatile! [])
          next-id (volatile! (:body-seq state))
          destroyed (volatile! 0)
          shatter! (fn [b f]
                     (when-not (:rubble b)
                       (vswap! destroyed + (count (:cells b))))
                     (vswap! events conj (into [:shatter]
                                               (conj (vec (:pos f)) (count (:cells b))))))]
      (doseq [b (:bodies state)]
        (if-let [f (get by-body (:body b))]
          (let [impact (- (or (:speed b) 0.0) (:speed f))]
            (cond
              (and (> impact SHATTER-SPEED) (> (count (:cells b)) 1))
              (do (shatter! b f)
                  (vswap! frags into (shatter-into b f @next-id))
                  (vswap! next-id + (count (:cells b))))

              ;; a body whose origin sank below the floor edge-on is a
              ;; physics blow-through: clear it whatever its sleep
              (< (second (:pos f)) -0.6)
              (shatter! b f)

              :else
              (let [slow (if (< (:speed f) SETTLE-SPEED)
                           (inc (or (:slow b) 0))
                           0)
                    ;; a long sub-threshold streak is settled stone even if
                    ;; contact churn keeps the solver's awake flag on
                    settled (and (not (:asleep f)) (>= slow SETTLE-SLOW-FRAMES))]
                (when settled
                  (vswap! events conj [:settle (:body b)]))
                (vswap! kept conj (assoc b :pos (:pos f) :quat (:quat f)
                                         :vel (:vel f)
                                         :speed (:speed f) :slow slow
                                         :asleep (or (:asleep f) settled))))))
          (vswap! kept conj b)))
      (let [base (assoc state
                        :bodies (into @kept @frags)
                        :body-seq @next-id
                        :events (into (:events state) @events)
                        :destroyed-cells (+ (or (:destroyed-cells state) 0) @destroyed))]
        (check-win (if (pos? @destroyed)
                     (assoc base :destruction (destruction-fraction base))
                     base))))))

(defn apply-physics
  "Fold one frame of Box3D facts into the world. Facts are plain data as
  reported by voxel.physics/step!: {:ball {:pos .. :speed .. :asleep ..} | nil,
  :bodies [{:body .. :pos .. :quat .. :speed .. :asleep ..}]}."
  [state {:keys [ball bodies]}]
  (-> state
      (apply-ball-fact ball)
      (apply-body-facts bodies)))

;; --- phase -------------------------------------------------------------------

(defn tick
  "Advance the world's phase bookkeeping one frame (win/lose/settle)."
  [state]
  (if (not= :playing (:phase state))
    state
    (let [quiet? (and (zero? (:balls-left state))
                      (nil? (:ball state))
                      (every? :asleep (:bodies state)))]
      (cond
        (>= (:destruction state) WIN-THRESHOLD)
        (assoc state :phase :won)

        quiet?
        (let [n (inc (or (:settle-frames state) 0))]
          (if (>= n SETTLE-FRAMES-TO-LOSE)
            (assoc state :phase :lost :settle-frames n)
            (assoc state :settle-frames n)))

        :else (assoc state :settle-frames 0)))))
