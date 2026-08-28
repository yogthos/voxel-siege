(ns voxel.terrain-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.terrain :as t]))

(defn- dist
  [a b]
  (Math/sqrt (+ (* (- (:x a) (:x b)) (- (:x a) (:x b)))
                (* (- (:z a) (:z b)) (- (:z a) (:z b))))))

(defn- pairs
  [xs]
  (for [i (range (count xs)) j (range i)]
    [(nth xs i) (nth xs j)]))

(deftest scene-is-deterministic
  (testing "the same seed always dresses the same scene"
    (is (= (t/scene 42) (t/scene 42))))
  (testing "a different seed dresses a different scene"
    (is (not= (t/scene 42) (t/scene 43)))))

(deftest scene-has-expected-shape
  (let [s (t/scene 7)]
    (testing "a full count of well-formed hills"
      (is (= t/HILL-COUNT (count (:hills s))))
      (is (every? #(and (number? (:x %)) (number? (:z %))
                        (> (:base-r %) 0.0) (> (:height %) 0.0))
                  (:hills s))))
    (testing "a full count of well-formed trees"
      (is (= t/TREE-COUNT (count (:trees s))))
      (is (every? #(and (number? (:x %)) (number? (:z %)) (> (:scale %) 0.0))
                  (:trees s))))
    (testing "the default seed fills out too"
      (is (= t/HILL-COUNT (count (:hills (t/scene)))))
      (is (= t/TREE-COUNT (count (:trees (t/scene))))))))

(deftest scenery-keeps-the-corridor-clear
  (let [s (t/scene 7)]
    (testing "no hill skirt reaches the firing corridor"
      (is (every? #(> (t/corridor-distance (:x %) (:z %)) (:base-r %))
                  (:hills s))))
    (testing "trees stand clear of the corridor"
      (is (every? #(> (t/corridor-distance (:x %) (:z %)) 3.0)
                  (:trees s))))))

(deftest scenery-stays-inside-the-field
  (let [s (t/scene 7)]
    (is (every? #(and (>= (:x %) t/FIELD-MIN-X) (<= (:x %) t/FIELD-MAX-X)
                      (>= (:z %) t/FIELD-MIN-Z) (<= (:z %) t/FIELD-MAX-Z))
                (concat (:hills s) (:trees s))))))

(deftest hills-do-not-overlap-each-other
  (let [hs (:hills (t/scene 7))]
    (is (every? (fn [[a b]] (> (dist a b) (+ (:base-r a) (:base-r b))))
                (pairs hs)))))

(deftest trees-avoid-hills-and-each-other
  (let [{:keys [hills trees]} (t/scene 7)]
    (testing "no tree stands inside a hill's skirt"
      (is (every? (fn [tr] (every? #(> (dist tr %) (+ (:base-r %) 1.5)) hills))
                  trees)))
    (testing "trees keep breathing room between them"
      (is (every? (fn [[a b]] (> (dist a b) 3.5))
                  (pairs trees))))))
