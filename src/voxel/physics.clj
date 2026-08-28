(ns voxel.physics
  "Box3D orchestration for the game. Owns the physics world plus the ball and
  the structure's rigid bodies, steps the simulation, and reports plain-data
  facts for voxel.world/apply-physics to fold in. Impure boundary — all game
  decisions (blast, split, shatter, scoring) stay in the pure voxel.world."
  (:require [voxel.box3d :as b3]
            [voxel.world :as w]))

(def ^:private GROUND-FRICTION 0.8)
(def ^:private CELL-DENSITY 2.0)
(def ^:private CELL-FRICTION 0.8)
(def ^:private CELL-RESTITUTION 0.05)
(def ^:private BALL-DENSITY 61.1)
(def ^:private BALL-FRICTION 0.4)
(def ^:private BALL-RESTITUTION 0.3)
(def ^:private SUBSTEPS 8)
(def ^:private EXPLOSION-FALLOFF 1.0)

(def ^:private world* (volatile! nil))
(def ^:private ball* (volatile! nil))
(def ^:private bodies* (volatile! {}))

(defn init!
  "Create the Box3D world with the game's gravity and a static ground box
  whose top surface sits at y = 0. Any previous world is replaced."
  []
  (when-let [old @world*] (b3/destroy-world! old))
  (let [wrld (b3/create-world 0.0 (- w/GRAVITY) 0.0 1)]
    (vreset! world* wrld)
    (vreset! ball* nil)
    (vreset! bodies* {})
    ;; thick static slab (top at y = 0, bottom at -10): nothing tunnels
    ;; through a 10m floor even at explosion launch speeds
    (let [ground (b3/create-body wrld b3/STATIC-BODY 0.0 -5.0 0.0 0.0 0.0 0.0 1.0 1)]
      (b3/add-box! ground 0.0 0.0 0.0 200.0 5.0 200.0 0.0 GROUND-FRICTION 0.0))
    wrld))

(defn spawn-ball!
  "Create the cannonball body at origin with velocity v. One live ball."
  [[ox oy oz] [vx vy vz]]
  (when-let [old @ball*] (b3/destroy-body! old))
  (let [id (b3/create-ball @world* ox oy oz w/BALL-RADIUS vx vy vz
                           BALL-DENSITY BALL-FRICTION BALL-RESTITUTION)]
    (vreset! ball* id)
    id))

(defn spawn-body!
  "Create a dynamic compound body: one unit box per cell, offset relative to
  `anchor`, posed at pos/quat. awake 0 spawns it sleeping — the standing
  castle costs nothing until disturbed — and blast-split children and
  shatter rubble spawn awake at the parent's pose so their world cells are
  unchanged. vel (optional [vx vy vz]) carries the impact velocity over so
  rubble sprays from the hit instead of freezing mid-air."
  ([pos quat awake anchor cells]
   (spawn-body! pos quat awake anchor cells nil))
  ([pos quat awake anchor cells vel]
   (let [[ax ay az] anchor
         [px py pz] pos
         [qx qy qz qw] quat
         id (b3/create-body @world* b3/DYNAMIC-BODY px py pz qx qy qz qw awake)]
     (doseq [[i j k] cells]
       ;; hulls are 0.98 wide (render size): a hair of daylight between
       ;; touching bodies stops coplanar face-grind without a visible gap
       (b3/add-box! id (- (+ i 0.5) ax) (- (+ j 0.5) ay) (- (+ k 0.5) az)
                    0.49 0.49 0.49 CELL-DENSITY CELL-FRICTION CELL-RESTITUTION))
     (when vel
       (b3/set-velocity! id (vel 0) (vel 1) (vel 2)))
     (vswap! bodies* assoc id :body)
     id)))

(defn explode!
  "Radial impulse at [x y z] reaching `radius` (decaying to zero over
  EXPLOSION-FALLOFF beyond it)."
  [[x y z] radius strength]
  (b3/explode! @world* x y z radius EXPLOSION-FALLOFF strength))

(defn sleep-body!
  "Force a body to sleep — Box3D settles its whole touching island. Used when
  the pure world declares a body settled after a long sub-threshold streak."
  [id]
  (b3/set-awake! id false))

(defn destroy-unreferenced!
  "Destroy physics bodies the world state no longer references (shattered or
  split-away parents, despawned balls)."
  [live-ids]
  (doseq [id (keys @bodies*)]
    (when-not (contains? live-ids id)
      (b3/destroy-body! id)
      (vswap! bodies* dissoc id)))
  (when (and @ball* (not (contains? live-ids @ball*)))
    (b3/destroy-body! @ball*)
    (vreset! ball* nil)))

(defn- body-fact
  [id]
  (let [[pos quat] (b3/transform id)
        [vx vy vz] (b3/velocity id)]
    {:body id
     :pos pos
     :quat quat
     :vel [vx vy vz]
     :speed (Math/sqrt (+ (* vx vx) (* vy vy) (* vz vz)))
     :asleep (not (b3/awake? id))}))

(defn step!
  "Advance physics by dt seconds, then report one fact per live body."
  [dt]
  (b3/step! @world* dt SUBSTEPS)
  {:ball (when-let [id @ball*] (body-fact id))
   :bodies (mapv body-fact (keys @bodies*))})
