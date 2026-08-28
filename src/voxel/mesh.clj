(ns voxel.mesh
  "Voxel surface meshes and the divergence-theorem volume/centroid algorithm.

  A voxel map is {[i j k] material}. Cell [i j k] occupies world space
  [i,i+1] x [j,j+1] x [k,k+1], volume exactly 1.

  The surface mesh is the set of quads between filled and empty cells.
  ")

;; --- surface extraction -------------------------------------------------------

(defn- cell-faces
  "The up-to-6 exposed face quads of cell [i j k], each [c0 c1 c2 c3] with
  outward winding. A face is exposed when its neighbour cell is empty."
  [voxels cell]
  (let [[i j k] cell
        x0 (double i) x1 (double (inc i))
        y0 (double j) y1 (double (inc j))
        z0 (double k) z1 (double (inc k))
        faces {[1 0 0]  [[x1 y0 z0] [x1 y1 z0] [x1 y1 z1] [x1 y0 z1]]
               [-1 0 0] [[x0 y0 z0] [x0 y0 z1] [x0 y1 z1] [x0 y1 z0]]
               [0 1 0]  [[x0 y1 z0] [x0 y1 z1] [x1 y1 z1] [x1 y1 z0]]
               [0 -1 0] [[x0 y0 z0] [x1 y0 z0] [x1 y0 z1] [x0 y0 z1]]
               [0 0 1]  [[x0 y0 z1] [x1 y0 z1] [x1 y1 z1] [x0 y1 z1]]
               [0 0 -1] [[x0 y0 z0] [x0 y1 z0] [x1 y1 z0] [x1 y0 z0]]}]
    (keep (fn [[dir quad]]
            (when-not (contains? voxels (mapv + cell dir))
              quad))
          faces)))

(defn surface-triangles
  "All triangles of the closed surface mesh of `voxels`, each [v0 v1 v2] of
  [x y z] doubles with outward winding. Two triangles per exposed face."
  [voxels]
  (into []
        (mapcat (fn [quad]
                  (let [[a b c d] quad]
                    [[a b c] [a c d]])))
        (mapcat (partial cell-faces voxels) (keys voxels))))

(defn surface-cells
  "Cells with at least one exposed face (interior cells are invisible)."
  [voxels]
  (into #{}
        (filter (fn [cell]
                  (some #(not (contains? voxels (mapv + cell %)))
                        [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])))
        (keys voxels)))

;; --- divergence-theorem volume + centroid -------------------------------------

(defn- tri-axis
  "Per-triangle data for axis work: the signed cross-product component `w` on
  the axis perpendicular to the F component, and the vertex values/edges on the
  axis itself."
  [axis [v0 v1 v2]]
  (let [[x0 y0 z0] v0
        d1 (mapv - v1 v0)
        d2 (mapv - v2 v0)
        [d1x d1y d1z] d1
        [d2x d2y d2z] d2]
    (case axis
      :x {:w (- (* d1y d2z) (* d1z d2y)) :a x0 :b (first d1) :c (first d2)}
      :y {:w (- (* d1z d2x) (* d1x d2z)) :a y0 :b (second d1) :c (second d2)}
      :z {:w (- (* d1x d2y) (* d1y d2x)) :a z0 :b (nth d1 2) :c (nth d2 2)})))

(defn mesh-volume
  "Signed volume of a closed triangle mesh via the divergence theorem:
  V = (1/6) SUM (d1 x d2)_x (x0 + x1 + x2). Outward winding => positive."
  [tris]
  (/ (reduce (fn [acc [v0 :as tri]]
               (let [{:keys [w]} (tri-axis :x tri)
                     xs (reduce + (map first tri))]
                 (+ acc (* w xs))))
             0.0
             tris)
     6.0))

(defn mesh-centroid
  "Centroid [x y z] of a closed triangle mesh, or nil for an empty mesh.

  Uses F = <x^2/2, 0, 0> (div F = x), so per triangle
  SUM_axis axis dV = w * (1/2) * INTEGRAL (a + b u + c v)^2 du dv over the unit
  param triangle, which expands exactly to
  a^2/2 + (b^2 + c^2)/12 + (a b + a c)/3 + b c / 12."
  [tris]
  (let [v (mesh-volume tris)]
    (when (not (zero? v))
      (mapv (fn [axis]
              (let [m (reduce (fn [acc tri]
                                (let [{:keys [w a b c]} (tri-axis axis tri)
                                      q (+ (* a a 0.5)
                                           (/ (+ (* b b) (* c c)) 12.0)
                                           (/ (+ (* a b) (* a c)) 3.0)
                                           (/ (* b c) 12.0))]
                                  (+ acc (* w 0.5 q))))
                              0.0
                              tris)]
                (/ m v)))
            [:x :y :z]))))
