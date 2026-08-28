(ns voxel.test-runner
  (:require [clojure.test :as t])
  (:gen-class))

(defn -main
  [& _]
  (require '[voxel.mesh-test])
  (require '[voxel.world-test])
  (let [{:keys [fail error]} (t/run-tests 'voxel.mesh-test 'voxel.world-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
