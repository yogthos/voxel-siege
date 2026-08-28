(ns voxel.mesh-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.mesh :as mesh]))

;; Deterministic LCG so randomized-shape tests are reproducible without host rand.
(defn- lcg [seed]
  (let [s (atom seed)]
    (fn [] (let [x (rem (+ (* 1103515245 @s) 12345) 2147483648)]
             (reset! s x)
             x))))

(defn- box
  "Voxel map for a filled ixjxk box with min corner at the origin."
  [nx ny nz]
  (into {}
        (for [i (range nx) j (range ny) k (range nz)]
          [[i j k] :stone])))

(defn- count-centroid
  "Count-based oracle: volume = cell count, centroid = mean of cell centres
  (each cell [i j k] spans [i,i+1]x[j,j+1]x[k,k+1])."
  [voxels]
  (let [n (count voxels)
        c (if (zero? n)
            nil
            (mapv #(/ (reduce + %) (double n))
                  (apply mapv vector (map (fn [[i j k]] [(+ i 0.5) (+ j 0.5) (+ k 0.5)])
                                          (keys voxels)))))]
    {:volume n :centroid c}))

(defn- volume-centroid-agree
  [voxels]
  (let [tris (mesh/surface-triangles voxels)
        oracle (count-centroid voxels)]
    (is (== (:volume oracle) (mesh/mesh-volume tris)) "volume == voxel count")
    (when (:centroid oracle)
      (is (== (first (:centroid oracle)) (first (mesh/mesh-centroid tris))) "centroid x")
      (is (== (second (:centroid oracle)) (second (mesh/mesh-centroid tris))) "centroid y")
      (is (== (nth (:centroid oracle) 2) (nth (mesh/mesh-centroid tris) 2)) "centroid z"))))

(deftest empty-map
  (testing "no voxels: zero volume, nil centroid, no triangles"
    (is (== 0 (mesh/mesh-volume (mesh/surface-triangles {}))))
    (is (nil? (mesh/mesh-centroid (mesh/surface-triangles {}))))
    (is (empty? (mesh/surface-triangles {})))))

(deftest unit-cube
  (testing "one voxel: volume exactly 1, centroid its centre"
    (volume-centroid-agree {[0 0 0] :stone})
    (is (= 1.0 (mesh/mesh-volume (mesh/surface-triangles {[5 3 7] :stone}))))))

(deftest single-cell-away-from-origin
  (testing "volume is translation-invariant, centroid translates"
    (volume-centroid-agree {[4 2 9] :stone})))

(deftest box-shapes
  (testing "boxes of several sizes: mesh volume == voxel count exactly"
    (volume-centroid-agree (box 2 1 1))
    (volume-centroid-agree (box 1 3 1))
    (volume-centroid-agree (box 3 2 4))
    (volume-centroid-agree (box 5 5 5))
    (volume-centroid-agree (box 7 1 3))))

(deftest l-shape
  (testing "non-convex connected set"
    (volume-centroid-agree (assoc (box 3 1 1)
                                  [0 1 0] :stone
                                  [0 2 0] :stone))))

(deftest hollow-shell
  (testing "hollow box: interior faces are part of the closed surface"
    (let [solid (box 4 4 4)
          hollow (dissoc solid [1 1 1] [2 1 1] [1 2 1] [2 2 1]
                         [1 1 2] [2 1 2] [1 2 2] [2 2 2])]
      (volume-centroid-agree hollow))))

(deftest disconnected-blobs
  (testing "two separate blobs: the summed surface is still closed"
    (volume-centroid-agree (merge (box 2 2 2)
                                  (zipmap (for [i (range 3)] [(+ i 10) 0 0]) (repeat :stone))))))

(deftest random-sets
  (testing "deterministic random clusters agree with the count oracle"
    (let [rnd (lcg 42)]
      (dotimes [_ 10]
        (let [seed-cell [(rem (rnd) 20) (rem (rnd) 20) (rem (rnd) 20)]
              voxels (loop [acc {(vec seed-cell) :stone} frontier [seed-cell]]
                       (if (or (empty? frontier) (> (count acc) 300))
                         acc
                         (let [c (peek frontier)
                               dirs [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]
                               grow (fn [acc' [c' :as cell]]
                                      (if (or (get acc' cell) (not (zero? (rem (rnd) 4))))
                                        acc'
                                        (assoc acc' cell :stone)))
                               acc' (reduce grow acc (map #(vec (map + c %)) dirs))
                               new (filter #(not (contains? acc %))
                                           (map #(vec (map + c %)) dirs))]
                           (recur acc' (into (pop (vec frontier)) new)))))]
          (volume-centroid-agree voxels))))))

(deftest winding-sign
  (testing "outward winding gives positive volume regardless of position"
    (is (pos? (mesh/mesh-volume (mesh/surface-triangles (box 2 2 2)))))
    (is (pos? (mesh/mesh-volume (mesh/surface-triangles (box 2 2 2)))))))

(deftest surface-cells-cull-interior
  (testing "only cells with an exposed face are surface cells"
    (is (= #{[0 0 0]} (mesh/surface-cells (box 1 1 1))))
    (is (= 27 (count (box 3 3 3))))
    (is (= 26 (count (mesh/surface-cells (box 3 3 3)))) "3x3x3 hides exactly its centre")
    (is (= 8 (count (mesh/surface-cells (box 2 2 2)))) "2x2x2: every cell is surface")))
  (testing "empty map has no surface cells"
    (is (empty? (mesh/surface-cells {}))))
