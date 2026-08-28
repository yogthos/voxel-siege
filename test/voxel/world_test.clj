(ns voxel.world-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.world :as w]))

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-6))

(defn- rect-slab
  [i-range j k-range]
  (into {} (for [i i-range k k-range] [[i j k] :stone])))

(defn- bar-cells
  [n j]
  (into {} (for [i (range n)] [[i j 0] :stone])))

(defn- body-fact
  [body pos quat speed asleep]
  {:body body :pos pos :quat quat :speed speed :asleep asleep})

(defn- test-body
  [id cells & {:keys [body asleep pos quat speed]
               :or {body 5 asleep false}}]
  (let [ks (keys cells)
        cnt (count ks)
        mean (fn [f] (/ (reduce + (map f ks)) cnt))
        anchor [(double (+ (mean first) 0.5))
                (double (+ (mean second) 0.5))
                (double (+ (mean #(nth % 2)) 0.5))]]
    {:id id :cells cells :anchor anchor :body body
     :pos (or pos anchor) :quat (or quat [0.0 0.0 0.0 1.0])
     :speed (or speed 0.0) :asleep asleep}))

(defn- bare-state
  [bodies initial-volume]
  (-> (w/initial-state)
      (assoc :bodies (vec bodies))
      (assoc :body-seq (inc (count bodies)))
      (assoc :initial-volume initial-volume)
      (assoc :events [])
      (assoc :destruction 0.0)))

(deftest initial-state-bodies
  (let [s (w/initial-state)
        parts (w/castle-parts)]
    (testing "the castle is three disjoint sleeping bodies from frame zero"
      (is (= 3 (count (:bodies s))))
      (is (every? :asleep (:bodies s)))
      (is (every? nil? (map :body (:bodies s))))
      (is (= 3 (count parts)))
      (is (= (map count parts) (map (comp count :cells) (:bodies s))))
      (is (empty? (apply clojure.set/intersection (map (comp set keys :cells) (:bodies s)))))
      (is (== (reduce + (map count parts)) (:initial-volume s))))
    (testing "each part is substantial"
      (is (> (count (first parts)) 50))
      (is (> (count (second parts)) 10))
      (is (> (count (nth parts 2)) 50)))))

(deftest initial-state-bookkeeping
  (let [s (w/initial-state)]
    (testing "fresh round bookkeeping"
      (is (= :playing (:phase s)))
      (is (= w/BALLS-PER-ROUND (:balls-left s)))
      (is (nil? (:ball s)))
      (is (zero? (w/destruction-fraction s)))
      (is (empty? (:events s))))))

(deftest aim-direction
  (testing "unit direction from yaw/pitch: yaw 0 points down -z"
    (let [[x y z] (w/dir-from-yaw-pitch 0.0 0.0)]
      (is (approx x 0))
      (is (approx y 0))
      (is (approx z -1))))
  (testing "45 degrees up tilts the shot"
    (let [[x y z] (w/dir-from-yaw-pitch 0.0 (/ Math/PI 4))]
      (is (approx x 0))
      (is (approx y (Math/sin (/ Math/PI 4))))
      (is (approx z (- (Math/cos (/ Math/PI 4))))))))

(deftest fire-creates-ball
  (let [s (w/initial-state)
        s2 (w/fire s (w/dir-from-yaw-pitch 0 0) 1.0)]
    (testing "ball spawns at the muzzle with power-scaled velocity, ammo decrements"
      (is (some? (:ball s2)))
      (is (= (dec w/BALLS-PER-ROUND) (:balls-left s2)))
      (is (= (:muzzle s) (:origin (:ball s2))))
      (is (approx (nth (:v (:ball s2)) 2) (- w/BALL-MAX-SPEED))))
    (testing "no double fire while a ball is live"
      (is (= s2 (w/fire s2 [0 0 -1] 1.0))))
    (testing "no firing with zero ammo"
      (let [dry (assoc s2 :ball nil :balls-left 0)]
        (is (= dry (w/fire dry [0 0 -1] 1.0)))))))

(deftest ball-impact-cell-detects-hits
  (let [vox {[0 0 0] :s [2 0 0] :s}]
    (testing "sphere centre inside an occupied cell hits it"
      (is (= [0 0 0] (w/ball-impact-cell vox [0.5 0.5 0.5] 0.5))))
    (testing "sphere surface overlapping a cell face hits it"
      (is (= [2 0 0] (w/ball-impact-cell vox [1.6 0.5 0.5] 0.5))))
    (testing "a miss in the gap between cells returns nil"
      (is (nil? (w/ball-impact-cell vox [1.5 0.5 0.5] 0.4))))
    (testing "empty grid never hits"
      (is (nil? (w/ball-impact-cell {} [0.5 0.5 0.5] 0.5))))))

(deftest blast-splits-body-along-the-cut
  (let [s (bare-state [(test-body 1 (bar-cells 3 0))] 3)
        s2 (w/blast-at s [1.5 0.5 0.5] 0.9)]
    (testing "destroying the middle cell splits the bar into two awake bodies"
      (is (= 2 (count (:bodies s2))))
      (is (= 1 (count (:cells (first (:bodies s2))))))
      (is (= 1 (count (:cells (second (:bodies s2))))))
      (is (every? (comp nil? :body) (:bodies s2)))
      (is (every? (comp false? :asleep) (:bodies s2)))
      (is (= 4 (:body-seq s2)))
      (is (some (fn [e] (and (= :blast (first e)) (== 1 (nth e 4)))) (:events s2)))
      (is (approx (w/destruction-fraction s2) (/ 1.0 3.0))))))

(deftest blast-split-inherits-parent-pose
  (let [posed (test-body 1 (bar-cells 3 0) :pos [10.0 4.0 -3.0] :quat [0.0 0.0 0.6 0.8] :body 77)
        s (bare-state [posed] 3)
        s2 (w/blast-at s [1.5 0.5 0.5] 0.9)]
    (testing "children keep the parent anchor position and rotation"
      (is (= 2 (count (:bodies s2))))
      (doseq [b (:bodies s2)]
        (is (= [10.0 4.0 -3.0] (:pos b)))
        (is (= [0.0 0.0 0.6 0.8] (:quat b)))
        (is (= (:anchor posed) (:anchor b)))))))

(deftest blast-misses-body-leaves-it-alone
  (let [s (bare-state [(test-body 1 (bar-cells 3 0) :body 42 :asleep true)] 3)
        s2 (w/blast-at s [50.0 50.0 50.0] 1.0)]
    (testing "an untouched body keeps its physics body and sleep"
      (is (= 1 (count (:bodies s2))))
      (is (= 42 (:body (first (:bodies s2)))))
      (is (:asleep (first (:bodies s2))))
      (is (empty? (:events s2)))
      (is (zero? (:destruction s2))))))

(deftest blast-can-destroy-a-body-entirely
  (let [s (bare-state [(test-body 1 {[0 0 0] :stone})] 1)
        s2 (w/blast-at s [0.5 0.5 0.5] 2.0)]
    (testing "total destruction removes the body"
      (is (empty? (:bodies s2)))
      (is (approx 1.0 (w/destruction-fraction s2))))))

(deftest blast-hits-multiple-bodies
  (let [a (test-body 1 {[0 0 0] :stone} :body 11)
        b (test-body 2 {[2 0 0] :stone} :body 22)
        s (bare-state [a b] 2)
        s2 (w/blast-at s [1.0 0.5 0.5] 1.6)]
    (testing "cells die across every body in range"
      (is (empty? (:bodies s2)))
      (is (approx 1.0 (w/destruction-fraction s2)))
      (is (== 2 (nth (first (filter #(= :blast (first %)) (:events s2))) 4))))))

(deftest attach-bodies-and-live-ids
  (let [s (-> (w/initial-state)
              (assoc :bodies [{:id 1 :body nil} {:id 2 :body nil}])
              (assoc :ball {:origin [0 3 22] :v [0 0 -1] :body nil :pos nil}))
        s2 (w/attach-bodies s {:bodies {1 101 2 202} :ball 900})]
    (testing "body ids attach to matching world bodies and the ball"
      (is (= 101 (:body (first (:bodies s2)))))
      (is (= 202 (:body (second (:bodies s2)))))
      (is (= 900 (:body (:ball s2)))))
    (testing "live ids cover the ball and every attached body"
      (is (= #{101 202 900} (w/live-body-ids s2))))))

(deftest ball-fact-impact-triggers-blast
  (let [wall (rect-slab (range -1 2) 1 (range -1 2))
        s (-> (bare-state [(test-body 1 wall)] (count wall))
              (assoc :blast-radius 1.2)
              (assoc :ball {:origin [0.0 1.5 5.0] :v [0 0 -40] :body 99 :pos nil}))
        s2 (w/apply-physics s {:ball {:pos [0.5 1.5 1.4] :speed 40.0 :asleep false}
                               :bodies []})]
    (testing "impact blasts the wall clears the ball records the event"
      (is (nil? (:ball s2)))
      (is (< (reduce + (map (comp count :cells) (:bodies s2))) (count wall)))
      (is (some (fn [e] (and (= :blast (first e)) (pos? (nth e 4)))) (:events s2)))
      (is (pos? (:destruction s2))))))

(deftest ball-fact-despawn-paths
  (testing "a vanished ball clears the world ball"
    (let [s (assoc (w/initial-state) :ball {:origin [0 3 5] :v [0 0 -1] :body 7 :pos [0 3 5]})]
      (is (nil? (:ball (w/apply-physics s {:ball nil :bodies []}))))))
  (testing "a ball below ground level despawns without blasting"
    (let [s (assoc (w/initial-state) :ball {:origin [0 30 5] :v [0 -1 0] :body 7 :pos nil})
          s2 (w/apply-physics s {:ball {:pos [0.5 0.2 3.0] :speed 20.0 :asleep false}
                                 :bodies []})]
      (is (nil? (:ball s2)))
      (is (empty? (:events s2)))))
  (testing "a sleeping ball despawns"
    (let [s (assoc (w/initial-state) :ball {:origin [0 3 5] :v [0 0 -1] :body 7 :pos nil})
          s2 (w/apply-physics s {:ball {:pos [0.5 0.6 3.0] :speed 0.0 :asleep true}
                                 :bodies []})]
      (is (nil? (:ball s2)))))
  (testing "an airborne ball fact updates the rendered position"
    (let [s (assoc (w/initial-state) :ball {:origin [0 3 22] :v [0 0 -40] :body 7 :pos nil})
          s2 (w/apply-physics s {:ball {:pos [0.0 3.2 18.0] :speed 40.0 :asleep false}
                                 :bodies []})]
      (is (= [0.0 3.2 18.0] (:pos (:ball s2)))))))

(deftest body-fact-falling-updates-pose
  (let [slab (rect-slab (range 2) 1 (range 2))
        s (bare-state [(test-body 1 slab :body 5 :pos [1.0 8.0 0.5] :speed 0.0)] (count slab))
        s2 (w/apply-physics s {:ball nil
                               :bodies [(body-fact 5 [1.0 6.0 0.5] [0 0 0 1] 7.0 false)]})]
    (testing "a falling body records the reported transform"
      (is (= 1 (count (:bodies s2))))
      (is (= [1.0 6.0 0.5] (:pos (first (:bodies s2)))))
      (is (= 7.0 (:speed (first (:bodies s2))))))))

(deftest body-fact-resting-stays-a-body
  (let [slab (rect-slab (range 2) 1 (range 2))
        s (bare-state [(test-body 1 slab :body 5 :pos [1.0 8.0 0.5] :speed 3.0)] (count slab))
        s2 (w/apply-physics s {:ball nil
                               :bodies [(body-fact 5 [1.0 0.5 0.5] [0 0 0 1] 0.0 true)]})]
    (testing "a resting body stays a rigid body"
      (is (= 1 (count (:bodies s2))))
      (is (= 5 (:body (first (:bodies s2)))))
      (is (:asleep (first (:bodies s2))))
      (is (zero? (w/destruction-fraction s2)))
      (is (empty? (filter #(= :shatter (first %)) (:events s2)))))))

(deftest body-fact-rotated-rest-keeps-rotation
  (let [sq2 (Math/sqrt 0.5)
        s (bare-state [(test-body 1 (bar-cells 2 1) :body 5 :pos [1.0 2.0 0.5] :speed 2.0)] 2)
        s2 (w/apply-physics s {:ball nil
                               :bodies [(body-fact 5 [1.0 1.0 0.5] [0.0 0.0 sq2 sq2] 0.0 true)]})]
    (testing "a rotated resting body holds its pose"
      (is (= [0.0 0.0 sq2 sq2] (:quat (first (:bodies s2)))))
      (is (= [1.0 1.0 0.5] (:pos (first (:bodies s2))))))))

(deftest body-fact-violent-impact-shatters
  (let [slab (rect-slab (range 2) 10 (range 2))
        s (bare-state [(test-body 1 slab :body 5 :pos [1.0 5.5 0.5] :speed 24.0)] (count slab))
        s2 (w/apply-physics s {:ball nil
                               :bodies [(body-fact 5 [1.0 0.55 0.5] [0 0 0 1] 0.1 false)]})]
    (testing "a sudden stop from above shatter speed breaks the body into rubble that stays"
      (is (= 4 (count (:bodies s2))))
      (is (every? #(= 1 (count (:cells %))) (:bodies s2)))
      (is (every? :rubble (:bodies s2)))
      (is (= (set (keys slab)) (set (mapcat #(keys (:cells %)) (:bodies s2)))))
      (is (every? (comp nil? :body) (:bodies s2)))
      (is (= 6 (:body-seq s2)))
      (is (approx 1.0 (w/destruction-fraction s2)))
      (is (== 1.0 (:destruction s2)))
      (is (some #(= :shatter (first %)) (:events s2))))))

(deftest rubble-shatter-does-not-double-count
  (let [slab (rect-slab (range 2) 10 (range 2))
        s (bare-state [(assoc (test-body 1 slab :body 5 :pos [1.0 5.5 0.5] :speed 24.0)
                              :rubble true)]
                      (count slab))
        s2 (w/apply-physics s {:ball nil
                               :bodies [(body-fact 5 [1.0 0.55 0.5] [0 0 0 1] 0.1 false)]})]
    (testing "a rubble body breaking apart again adds no destroyed cells"
      (is (= 4 (count (:bodies s2))))
      (is (every? :rubble (:bodies s2)))
      (is (zero? (:destroyed-cells s2)))
      (is (zero? (w/destruction-fraction s2))))))

(deftest blast-on-rubble-carves-without-recounting
  (let [s (bare-state [(assoc (test-body 1 (bar-cells 3 0)) :rubble true)] 3)
        s2 (w/blast-at s [1.5 0.5 0.5] 0.9)]
    (testing "carving rubble removes cells but they were already counted destroyed"
      (is (= 2 (count (:bodies s2))))
      (is (every? :rubble (:bodies s2)))
      (is (zero? (:destroyed-cells s2)))
      (is (zero? (w/destruction-fraction s2)))
      (is (some #(= :blast (first %)) (:events s2))))))

(deftest single-cell-body-survives-hard-impact
  (let [s (bare-state [(test-body 1 {[0 0 0] :stone} :body 5 :pos [0.5 5.5 0.5] :speed 24.0)] 1)
        s2 (w/apply-physics s {:ball nil
                               :bodies [(body-fact 5 [0.5 0.55 0.5] [0 0 0 1] 0.1 false)]})]
    (testing "a lone block landing hard just stops - no shatter respawn churn"
      (is (= 1 (count (:bodies s2))))
      (is (= 5 (:body (first (:bodies s2)))))
      (is (zero? (:destroyed-cells s2)))
      (is (empty? (filter #(= :shatter (first %)) (:events s2)))))))

(deftest rubble-fragments-inherit-pose-and-velocity
  (let [slab (rect-slab (range 2) 10 (range 2))
        posed (test-body 1 slab :body 5 :pos [1.0 5.5 0.5] :speed 24.0 :quat [0.0 0.0 0.6 0.8])
        s (bare-state [posed] (count slab))
        fact (assoc (body-fact 5 [2.0 0.55 1.5] [0.0 0.0 0.6 0.8] 0.1 false)
                    :vel [3.0 -12.0 0.5])
        s2 (w/apply-physics s {:ball nil :bodies [fact]})]
    (testing "fragments spawn at the impact pose carrying the impact velocity"
      (is (= 4 (count (:bodies s2))))
      (doseq [b (:bodies s2)]
        (is (= [2.0 0.55 1.5] (:pos b)))
        (is (= [0.0 0.0 0.6 0.8] (:quat b)))
        (is (= (:anchor posed) (:anchor b)))
        (is (= [3.0 -12.0 0.5] (:vel b)))))))

(deftest body-fact-resting-below-ground-destroyed
  (let [s (bare-state [(test-body 1 {[0 5 0] :stone} :body 5 :pos [0.5 5.5 0.5] :speed 1.0)] 1)
        s2 (w/apply-physics s {:ball nil
                               :bodies [(body-fact 5 [0.5 -0.9 0.5] [0 0 0 1] 0.0 true)]})]
    (testing "a body wedged below the ground plane is destroyed"
      (is (empty? (:bodies s2)))
      (is (approx 1.0 (w/destruction-fraction s2)))
      (is (some #(= :shatter (first %)) (:events s2))))))

(deftest win-condition
  (let [wall (rect-slab (range -1 2) 1 (range -1 2))
        s (bare-state [(test-body 1 wall)] (count wall))]
    (testing "crossing the destruction threshold wins immediately"
      (is (= :won (:phase (w/blast-at s [0.0 1.5 0.0] 10.0)))))))

(deftest shatter-can-win
  (let [slab (rect-slab (range 2) 10 (range 2))
        s (-> (bare-state [(test-body 1 slab :body 5 :pos [1.0 5.5 0.5] :speed 24.0)] (count slab))
              (assoc :destruction 0.65))
        s2 (w/apply-physics s {:ball nil
                               :bodies [(body-fact 5 [1.0 0.55 0.5] [0 0 0 1] 0.0 false)]})]
    (testing "a shatter that pushes destruction over the threshold wins"
      (is (= :won (:phase s2))))))

(deftest lose-condition
  (let [s (-> (w/initial-state)
              (assoc :balls-left 0)
              (assoc :settle-frames w/SETTLE-FRAMES-TO-LOSE))]
    (testing "out of ammo nothing flying all rubble asleep: lose"
      (is (= :lost (:phase (w/tick s)))))
    (testing "tumbling bodies postpone the loss"
      (let [flying (assoc s :bodies [(test-body 1 {[0 0 0] :stone} :body 5
                                             :pos [0 5 0] :speed 9.0 :asleep false)])]
        (is (= :playing (:phase (w/tick flying))))))))

(deftest components-split-disconnected-cells
  (testing "two separated blobs are two components"
    (is (= 2 (count (w/components {[0 0 0] :s [5 5 5] :s})))))
  (testing "a bridge merges them into one"
    (is (= 1 (count (w/components {[0 0 0] :s [1 0 0] :s [2 0 0] :s [3 0 0] :s [4 0 0] :s [5 0 0] :s}))))))

(deftest trajectory-preview-terminates
  (let [pts (w/trajectory-points [0.0 3.0 0.0] [0.0 5.0 -20.0] (/ 1.0 60.0))]
    (testing "preview is non-empty and ends at/below ground"
      (is (<= 2 (count pts)))
      (is (>= 0 (second (last pts)))))))

(deftest body-settles-after-slow-streak
  (let [slab (rect-slab (range 2) 1 (range 2))
        s0 (bare-state [(test-body 1 slab :body 5 :pos [1.0 0.5 0.5] :speed 3.0)] (count slab))
        slow (body-fact 5 [1.0 0.5 0.5] [0 0 0 1] 0.04 false)
        n w/SETTLE-SLOW-FRAMES
        ;; one spike mid-streak resets the counter
        spike-at (int (/ n 2))
        step-slow (fn [s i speed]
                    (w/apply-physics s {:ball nil
                                        :bodies [(body-fact 5 [1.0 0.5 0.5] [0 0 0 1] speed false)]}))
        run-spike (fn [s i] (step-slow s i (if (== i spike-at) 2.0 0.04)))
        run-clean (fn [s i] (step-slow s i 0.04))
        with-spike (loop [s s0 i 0] (if (>= i n) s (recur (run-spike s i) (inc i))))
        clean (loop [s s0 i 0] (if (>= i n) s (recur (run-clean s i) (inc i))))
        settle-ev (fn [s] (some #(= :settle (first %)) (:events s)))]
    (testing "a long enough slow streak settles the body and emits a settle event"
      (is (settle-ev clean))
      (is (:asleep (first (:bodies clean))))
      (is (== 0.04 (:speed (first (:bodies clean))))))
    (testing "a speed spike resets the streak - no settle one frame early"
      (is (nil? (settle-ev with-spike)))
      (is (not (:asleep (first (:bodies with-spike))))))))

(deftest ball-settles-after-slow-streak
  (let [mk (fn [slow] (assoc (w/initial-state)
                             :ball {:origin [0 3 22] :v [0 0 -40] :body 7
                                    :pos [0 0.6 3.0] :slow slow}))
        roll (fn [s] (w/apply-physics s {:ball {:pos [0.5 0.6 3.0] :speed 0.05 :asleep false}
                                         :bodies []}))]
    (testing "a ball rolling below settle speed for the streak despawns"
      (is (nil? (:ball (loop [s (mk 0) i 0]
                         (if (>= i w/SETTLE-SLOW-FRAMES) s (recur (roll s) (inc i))))))))
    (testing "one short of the streak it is still live"
      (is (some? (:ball (loop [s (mk 0) i 0]
                          (if (>= i (dec w/SETTLE-SLOW-FRAMES)) s (recur (roll s) (inc i))))))))))
