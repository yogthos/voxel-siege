(ns voxel.b3-probe
  "Native smoke test for the Box3D shim (voxel.box3d / native/voxel_b3.c).
  Verifies through the whole FFI chain: world creation, gravity, ground
  contact, sleeping, explosions, and ball ballistics. Exits 0/1.
  Run: jolt -M:b3probe"
  (:require [voxel.box3d :as b3]
            [voxel.physics :as phys]))

(def ^:private dt 0.016666668)

(defn- near [a b tol]
  (let [d (- a b)] (< (if (neg? d) (- d) d) tol)))

(defn- falls-under-gravity [w]
  (let [box (b3/create-body w b3/DYNAMIC-BODY 0.0 6.0 0.0 0.0 0.0 0.0 1.0 1)]
    (b3/add-box! box 0.0 0.0 0.0 0.5 0.5 0.5 1.0 0.6 0.05)
    (dotimes [_ 30] (b3/step! w dt 4))
    (let [[[_ y _]] (b3/transform box)]
      [box (< y 5.0) y])))

(defn- settles-and-sleeps [w box]
  (dotimes [_ 210] (b3/step! w dt 4))
  (let [[[_ y _] _] (b3/transform box)
        [vx vy vz] (b3/velocity box)
        speed (Math/sqrt (+ (* vx vx) (* vy vy) (* vz vz)))]
    [(near y 0.5 0.05) (< speed 0.05) (not (b3/awake? box)) y speed]))

(defn- explosion-wakes [w]
  (let [box (b3/create-body w b3/DYNAMIC-BODY 0.0 1.0 2.5 0.0 0.0 0.0 1.0 1)]
    (b3/add-box! box 0.0 0.0 0.0 0.5 0.5 0.5 1.0 0.6 0.05)
    (dotimes [_ 240] (b3/step! w dt 4)) ; settle+sleep at z=2.5
    (when (b3/awake? box) (throw (ex-info "box did not sleep" {})))
    (b3/explode! w 0.0 0.5 0.0 2.0 1.0 3.0)
    (b3/step! w dt 4)
    (let [awoke (b3/awake? box)]
      (dotimes [_ 30] (b3/step! w dt 4))
      (let [[[_ y _] _] (b3/transform box)]
        (b3/destroy-body! box)
        [awoke (or (> y 1.2) (< y 0.8)) y]))))

(defn- ball-flies [w]
  (let [ball (b3/create-ball w 0.0 3.0 22.0 0.5 0.0 10.0 -40.0 1.0 0.6 0.3)]
    (dotimes [_ 60] (b3/step! w dt 4))
    (let [[[x y z] _] (b3/transform ball)
          [_ vy _] (b3/velocity ball)]
      (b3/destroy-body! ball)
      [(< z 0.0) (< vy 10.0) z y])))

(defn -main [& _]
  (let [w (b3/create-world 0.0 -25.0 0.0 1)
        ground (b3/create-body w b3/STATIC-BODY 0.0 -0.5 0.0 0.0 0.0 0.0 1.0 1)]
    (b3/add-box! ground 0.0 0.0 0.0 200.0 0.5 200.0 0.0 0.8 0.0)
    (let [fall (falls-under-gravity w)
          box (first fall)
          settle (settles-and-sleeps w box)
          boom (explosion-wakes w)
          ball (ball-flies w)
          ;; world recycle: destroy a world that still has live bodies, then
          ;; make a fresh one (the game's restart path)
          recycle (do (b3/destroy-world! w)
                      (let [w2 (b3/create-world 0.0 -25.0 0.0 1)]
                        [(pos? w2) w2]))
          ;; orchestration restart: init! twice -- the second call runs the
          ;; destroy-old-world path (regression: crashed the game on R /
          ;; play-again with "Unknown class b3" when physics.clj called a
          ;; nonexistent b3/destroy-world)
          restart (let [p1 (phys/init!)
                        p2 (phys/init!)]
                    [(and (pos? p1) (pos? p2)) p1 p2])]
      (println "world id         " w)
      (println "fell under g     " (second fall) "y=" (nth fall 2))
      (println "settled asleep   " settle)
      (println "explosion pushed " boom)
      (println "ball flew        " ball)
      (println "world recycled   " (first recycle) (second recycle))
      (println "physics restart  " (first restart) (nth restart 1) (nth restart 2))
      (let [failures
            (concat
             (when-not (pos? w) [:world-id])
             (when-not (second fall) [:no-gravity])
             (when-not (and (first settle) (second settle) (nth settle 2)) [:no-settle])
             (when-not (and (first boom) (second boom)) [:no-explosion])
             (when-not (and (first ball) (second ball)) [:no-ball])
             (when-not (first recycle) [:no-world-recycle])
             (when-not (first restart) [:no-physics-restart]))]
        (if (seq failures)
          (do (println "FAIL:" failures) (System/exit 1))
          (do (println "b3probe PASS") (System/exit 0)))))))
