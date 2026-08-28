(ns voxel.world-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.world :as w]))

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-6))

(defn- wall
  "3x3x3 wall of cells at i in -1..1, j 0..2, k 0..2."
  []
  (into {}
        (for [i [-1 0 1] j (range 3) k (range 3)]
          [[i j k] :stone])))
;; 27 cells total: i in {-1,0,1}, j in 0..2, k in 0..2.

(deftest initial-state-shape
  (testing "the castle level is a grounded, non-trivial voxel set"
    (let [s (w/initial-state)]
      (is (= :playing (:phase s)))
      (is (= w/BALLS-PER-ROUND (:balls-left s)))
      (is (pos? (count (:voxels s))))
      (is (pos? (count (w/grounded-cells (:voxels s)))) "some cells sit on the ground plane")
      (is (== (count (:voxels s)) (:initial-volume s))
          "initial volume (mesh-computed) equals the voxel count")
      (is (nil? (:ball s)))
      (is (zero? (w/destruction-fraction s))))))

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
      (is (= (:muzzle s) [(:x (:ball s2)) (:y (:ball s2)) (:z (:ball s2))]))
      (is (approx (:vz (:ball s2)) (- w/BALL-MAX-SPEED))))
    (testing "no double fire while a ball is live"
      (is (= s2 (w/fire s2 [0 0 -1] 1.0))))
    (testing "no firing with zero ammo"
      (let [dry (assoc s2 :ball nil :balls-left 0)]
        (is (= dry (w/fire dry [0 0 -1] 1.0)))))))

(deftest ball-flies-ballistically
  (let [s  (-> (w/initial-state)
               (assoc :muzzle [0.0 3.0 0.0])
               (assoc :voxels {})
               (assoc :initial-volume 1)
               (w/fire (w/dir-from-yaw-pitch 0 0) 1.0))
        s3 (w/step s (/ 1.0 60.0))]
    (testing "straight shot drops by gravity and advances downrange"
      (is (approx (:z (:ball s3)) (- 0.0 (* w/BALL-MAX-SPEED (/ 1.0 60.0)))))
      (is (approx (:vy (:ball s3)) (- (* w/GRAVITY (/ 1.0 60.0))))))))

(deftest ground-impact-kills-ball
  (let [s (-> (w/initial-state)
              (assoc :muzzle [0.0 3.0 0.0])
              (assoc :voxels {})
              (assoc :initial-volume 1)
              (w/fire [0.0 (- 0.2) -1.0] 1.0))]
    (testing "a ball aimed at empty ground despawns without touching voxels"
      (let [end (loop [s s n 0]
                  (if (or (nil? (:ball s)) (> n 600))
                    s
                    (recur (w/step s (/ 1.0 60.0)) (inc n))))]
        (is (nil? (:ball end)))))))

(deftest blast-destroys-and-scores
  (let [s  (-> (w/initial-state)
               (assoc :muzzle [0.0 1.5 5.0])
               (assoc :voxels (wall))
               (assoc :initial-volume 27)
               (assoc :blast-radius 1.2))
        s2 (w/fire s (w/dir-from-yaw-pitch 0 0) 1.0)
        end (loop [s s2 n 0]
              (if (or (nil? (:ball s)) (> n 600))
                s
                (recur (w/step s (/ 1.0 60.0)) (inc n))))]
    (testing "impact removes the cells near the hit and leaves distant ones"
      (is (nil? (:ball end)))
      (is (pos? (count (:voxels end))) "outer cells beyond the radius survive")
      (is (< (count (:voxels end)) 27) "cells near the impact are destroyed")
      (is (approx (w/destruction-fraction end)
                  (/ (- 27 (count (:voxels end))) 27.0)))
      (is (some (fn [e] (and (= :blast (first e)) (pos? (nth e 3))))
                (:events end))
          "a blast event with the destroyed count is recorded"))))

(deftest components-and-grounding
  (testing "two separated blobs are two components"
    (is (= 2 (count (w/components {[0 0 0] :s [5 5 5] :s})))))
  (testing "a bridge merges them into one"
    (is (= 1 (count (w/components {[0 0 0] :s [1 0 0] :s [2 0 0] :s [3 0 0] :s [4 0 0] :s [5 0 0] :s})))))
  (testing "grounded cells are those reachable from the ground plane"
    (is (= #{[0 0 0] [0 1 0]} (w/grounded-cells {[0 0 0] :s [0 1 0] :s})))
    (is (= #{} (w/grounded-cells {[0 5 0] :s})) "a floating cell is not grounded")))

(deftest cutting-support-detaches-chunk
  (let [table (merge (into {} (for [i (range 3) k (range 3)] [[i 2 k] :stone]))
                     {[0 0 0] :stone [0 1 0] :stone})
        s  (-> (w/initial-state)
               (assoc :voxels table)
               (assoc :initial-volume (count table)))
        s2 (w/blast-at s [0.5 0.5 0.5] 1.6)]
    (testing "the slab detaches as a falling chunk and leaves the static grid"
      (is (= 1 (count (:chunks s2))))
      (is (= 9 (count (:cells (first (:chunks s2))))))
      (is (empty? (:voxels s2)) "leg destroyed, slab no longer static"))))

(deftest chunk-falls-and-shatters
  (let [slab (into {} (for [i (range 2) k (range 2)] [[i 10 k] :stone]))
        s (-> (w/initial-state)
              (assoc :voxels {})
              (assoc :initial-volume 4)
              (assoc :chunks [{:cells slab :off 0.0 :vy 0.0}]))
        end (loop [s s n 0]
              (if (or (empty? (:chunks s)) (> n 400))
                s
                (recur (w/step s (/ 1.0 60.0)) (inc n))))]
    (testing "a hard landing destroys the chunk (it never becomes rubble)"
      (is (empty? (:chunks end)))
      (is (empty? (:voxels end)))
      (is (approx 1.0 (w/destruction-fraction end)))
      (is (some #(= :shatter (first %)) (:events end))
          "a shatter event is recorded"))))

(deftest chunk-falls-and-merges-as-rubble
  (let [slab (into {} (for [i (range 2) k (range 2)] [[i 1 k] :stone]))
        s (-> (w/initial-state)
              (assoc :voxels {})
              (assoc :initial-volume 4)
              (assoc :chunks [{:cells slab :off 0.0 :vy 0.0}]))
        end (loop [s s n 0]
              (if (or (empty? (:chunks s)) (> n 400))
                s
                (recur (w/step s (/ 1.0 60.0)) (inc n))))]
    (testing "a soft landing merges the chunk into the static grid, grounded"
      (is (empty? (:chunks end)))
      (is (= 4 (count (:voxels end))))
      (is (= #{[0 0 0] [1 0 0] [0 0 1] [1 0 1]} (set (keys (:voxels end)))))
      (is (zero? (w/destruction-fraction end))))))

(deftest win-condition
  (let [s (-> (w/initial-state)
              (assoc :voxels (wall))
              (assoc :initial-volume 9)
              (assoc :blast-radius 10.0))]
    (testing "crossing the destruction threshold wins immediately"
      (let [s2 (w/blast-at s [0.5 1.5 1.5] 10.0)]
        (is (= :won (:phase s2)))))))

(deftest destruction-cached-in-state
  (testing "initial state carries a zero cached fraction"
    (is (zero? (:destruction (w/initial-state)))))
  (testing "a blast refreshes the cached fraction to the computed value"
    (let [hit (w/blast-at (w/initial-state) [3.5 1.5 -22.5] 2.0)]
      (is (pos? (:destruction hit)))
      (is (== (w/destruction-fraction hit) (:destruction hit)))))
  (testing "a chunk landing (shatter or rubble) refreshes it too"
    (let [slab (into {} (for [i (range 2) k (range 2)] [[i 10 k] :stone]))
          s (-> (w/initial-state)
                (assoc :voxels {})
                (assoc :initial-volume 4)
                (assoc :chunks [{:cells slab :volume 4 :off 0.0 :vy 0.0}]))
          end (loop [st s n 0]
                (if (or (empty? (:chunks st)) (> n 400))
                  st
                  (recur (w/step st (/ 1.0 60.0)) (inc n))))]
      (is (== 1.0 (:destruction end)))
      (is (== (w/destruction-fraction end) (:destruction end)))))
  (testing "frames without structural change keep the cached value in sync"
    (let [flown (loop [st (w/fire (w/initial-state) (w/dir-from-yaw-pitch 0 0.4) 0.9) n 0]
                  (if (or (nil? (:ball st)) (> n 300))
                    st
                    (recur (w/step st (/ 1.0 60.0)) (inc n))))]
      (is (== (w/destruction-fraction flown) (:destruction flown))))))

(deftest lose-condition
  (let [s (-> (w/initial-state)
              (assoc :voxels (wall))
              (assoc :initial-volume 9)
              (assoc :balls-left 0)
              (assoc :blast-radius 0.5)
              (assoc :settle-frames w/SETTLE-FRAMES-TO-LOSE))]
    (testing "out of ammo, nothing flying, settled: lose"
      (is (= :lost (:phase (w/step s (/ 1.0 60.0))))))))

(deftest trajectory-preview-terminates
  (let [pts (w/trajectory-points [0.0 3.0 0.0] [0.0 5.0 -20.0] (/ 1.0 60.0))]
    (testing "preview is non-empty and ends at/below ground"
      (is (<= 2 (count pts)))
      (is (>= 0 (second (last pts))) "last point reaches y <= 0"))))
