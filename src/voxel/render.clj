(ns voxel.render
  "Raylib draw calls ONLY. Draws a world state (voxel.world) plus the UI
  overlay. No game logic here."
  (:require [voxel.raylib :as rl]
            [voxel.mesh :as mesh]
            [voxel.world :as w]
            [voxel.input :as input]))

;; player vantage: behind and above the muzzle, looking down the -z range
(def CAMERA-POS [0.0 8.0 34.0])
(def CAMERA-TARGET [0.0 3.0 -20.0])

(def MAT-COLORS
  {:stone (rl/rgba 148 151 165 255)
   :stone-dark (rl/rgba 103 106 120 255)
   :gold rl/GOLD})

(defn- cell-color
  "Material colour with a deterministic per-cell tint so walls do not read
  as flat slabs."
  [cell mat]
  (let [[i j k] cell
        v (mod (+ i j k) 3)
        f (case v 0 1.0 1 0.93 2 0.86)]
    (rl/shade (get MAT-COLORS mat) f)))

(defn- draw-chunk!
  "A structure body at its physics transform: translate to the body position,
  rotate by its quaternion, and draw the cells relative to the anchor the
  body was spawned around."
  [ch]
  (let [[px py pz] (:pos ch)
        [ax ay az] (:anchor ch)
        cells (:cells ch)]
    (rl/rl-push-matrix)
    (rl/rl-translate-f (double px) (double py) (double pz))
    (rl/rl-rotate-quaternion! (:quat ch))
    (doseq [cell (mesh/surface-cells cells)]
      (let [[i j k] cell]
        (rl/cube! :pos [(- (+ i 0.5) ax) (- (+ j 0.5) ay) (- (+ k 0.5) az)]
                  :size 0.98
                  :color (cell-color cell (get cells cell)))))
    (rl/rl-pop-matrix)))

;; --- terrain (Tunic-style pastel scenery) ------------------------------------

(def GRASS-A (rl/rgba 156 199 129 255))
(def GRASS-B (rl/rgba 146 188 121 255))
(def HILL-SIDE (rl/rgba 122 166 103 255))
(def HILL-CAP (rl/rgba 171 209 137 255))
(def PINE-BOTTOM (rl/rgba 47 81 57 255))
(def PINE-MID (rl/rgba 62 100 70 255))
(def PINE-TOP (rl/rgba 84 128 86 255))
(def TRUNK-BROWN (rl/rgba 110 84 60 255))

(def ^:private FIELD-X0 -66.0)
(def ^:private FIELD-X1 66.0)
(def ^:private FIELD-Z0 -66.0)
(def ^:private FIELD-Z1 42.0)
(def ^:private TILE 2.0)
(def ^:private CHK-X0 -40.0)
(def ^:private CHK-X1 40.0)
(def ^:private CHK-Z0 -56.0)
(def ^:private CHK-Z1 36.0)

(defn- draw-ground!
  "One big grass quad plus a soft two-tone checker a hair above it — scale
  cues without the old hard grid."
  []
  (rl/rl-begin rl/RL-TRIANGLES)
  ;; triangles are wound +y (camera looks down): raylib culls back-faces
  (rl/rl-color! GRASS-A)
  (rl/rl-vertex-3f FIELD-X0 0.0 FIELD-Z0)
  (rl/rl-vertex-3f FIELD-X1 0.0 FIELD-Z1)
  (rl/rl-vertex-3f FIELD-X1 0.0 FIELD-Z0)
  (rl/rl-vertex-3f FIELD-X0 0.0 FIELD-Z0)
  (rl/rl-vertex-3f FIELD-X0 0.0 FIELD-Z1)
  (rl/rl-vertex-3f FIELD-X1 0.0 FIELD-Z1)
  (rl/rl-color! GRASS-B)
  (let [y 0.008
        i0 (int (/ CHK-X0 TILE)) i1 (int (/ CHK-X1 TILE))
        j0 (int (/ CHK-Z0 TILE)) j1 (int (/ CHK-Z1 TILE))]
    (doseq [i (range i0 i1)
            j (range j0 j1)
            :when (odd? (+ i j))]
      (let [x0 (* i TILE) z0 (* j TILE)
            x1 (+ x0 TILE) z1 (+ z0 TILE)]
        (rl/rl-vertex-3f x0 y z0)
        (rl/rl-vertex-3f x1 y z1)
        (rl/rl-vertex-3f x1 y z0)
        (rl/rl-vertex-3f x0 y z0)
        (rl/rl-vertex-3f x0 y z1)
        (rl/rl-vertex-3f x1 y z1))))
  (rl/rl-end))

(defn- draw-hill!
  "Two stacked flat-topped tiers — a low plateau mound."
  [{:keys [x z base-r height]}]
  (rl/frustum! :pos [x 0.0 z]
               :base-r base-r :top-r (* 0.55 base-r)
               :height (* 0.6 height) :segments 12 :color HILL-SIDE)
  (rl/frustum! :pos [x (* 0.6 height) z]
               :base-r (* 0.5 base-r) :top-r (* 0.16 base-r)
               :height (* 0.5 height) :segments 10 :color HILL-CAP))

(defn- draw-tree!
  "A trunk plus three stepped pine tiers."
  [{:keys [x z scale]}]
  (let [s scale]
    (rl/cube! :pos [x (* 0.45 s) z] :size [(* 0.5 s) (* 0.9 s) (* 0.5 s)]
              :color TRUNK-BROWN)
    (rl/frustum! :pos [x (* 0.7 s) z] :base-r (* 1.4 s) :top-r (* 0.55 s)
                 :height (* 1.5 s) :segments 8 :color PINE-BOTTOM)
    (rl/frustum! :pos [x (* 1.7 s) z] :base-r (* 1.05 s) :top-r (* 0.42 s)
                 :height (* 1.3 s) :segments 8 :color PINE-MID)
    (rl/frustum! :pos [x (* 2.6 s) z] :base-r (* 0.75 s) :top-r (* 0.12 s)
                 :height (* 1.15 s) :segments 7 :color PINE-TOP)))

(defn- draw-terrain!
  "The static scenery: grass field, hills, pines."
  [terrain]
  (draw-ground!)
  (doseq [h (:hills terrain)] (draw-hill! h))
  (doseq [t (:trees terrain)] (draw-tree! t)))

(defn- draw-trajectory!
  "Dotted aim arc from the muzzle with the current aim and power."
  [aim power]
  (let [[yaw pitch] aim
        dir (w/dir-from-yaw-pitch yaw pitch)
        speed (+ w/BALL-MIN-SPEED (* power (- w/BALL-MAX-SPEED w/BALL-MIN-SPEED)))
        pts (w/trajectory-points w/MUZZLE (mapv #(* % speed) dir) (/ 1.0 30.0))]
    (rl/rl-begin rl/RL-LINES)
    (rl/rl-color! rl/WHITE)
    (doseq [[[x1 y1 z1] [x2 y2 z2]] (partition 2 2 pts)]
      (rl/rl-vertex-3f (double x1) (double y1) (double z1))
      (rl/rl-vertex-3f (double x2) (double y2) (double z2)))
    (rl/rl-end)))

(defn- draw-debris!
  [debris]
  (doseq [{:keys [x y z size color]} debris]
    (rl/cube! :pos [x y z] :size size :color color)))

(defn- draw-hud!
  [world ui width height]
  (let [pct (:destruction world)                 ; cached by voxel.world — recomputing it here costs ~8ms/frame
        destr (int (* 100 pct))
        bar-w 300
        bar-x (- (/ width 2) (/ bar-w 2))]
    ;; destruction bar
    (rl/text! (str "DESTRUCTION " destr "%")
              :x (- (/ width 2) 80) :y 14 :size 22 :color rl/WHITE)
    (rl/rect! :x bar-x :y 40 :width bar-w :height 12 :color rl/DARKGRAY)
    (rl/rect! :x bar-x :y 40 :width (int (* bar-w (min 1.0 pct))) :height 12
              :color (if (>= pct w/WIN-THRESHOLD) rl/GREEN rl/ORANGE))
    ;; ammo
    (dotimes [n (:balls-left world)]
      (rl/circle! :x (+ 24 (* n 34)) :y (- height 26) :radius 11 :color rl/DARKBROWN))
    (rl/text! (str "SHOTS LEFT " (:balls-left world))
              :x 14 :y (- height 58) :size 18 :color rl/WHITE)
    ;; power meter while charging
    (when (:charging ui)
      (let [p (rl/rgba 255 203 0 255)
            h (int (* 120 (min 1.0 (/ (:charge-t ui) input/CHARGE-TIME))))]
        (rl/rect! :x (- width 46) :y (- height 160) :width 26 :height 120 :color rl/DARKGRAY)
        (rl/rect! :x (- width 46) :y (- height 40 h) :width 26 :height h :color p)))
    ;; hint
    (rl/text! "hold LMB to charge - release to fire - R restart"
              :x 14 :y (- height 24) :size 16 :color rl/LIGHTGRAY)))

(defn- draw-crosshair!
  [mx my]
  (rl/line! :x1 (- mx 12) :y1 my :x2 (- mx 4) :y2 my :color rl/WHITE)
  (rl/line! :x1 (+ mx 4) :y1 my :x2 (+ mx 12) :y2 my :color rl/WHITE)
  (rl/line! :x1 mx :y1 (- my 12) :x2 mx :y2 (- my 4) :color rl/WHITE)
  (rl/line! :x1 mx :y1 (+ my 4) :x2 mx :y2 (+ my 12) :color rl/WHITE))

(defn- draw-overlay!
  [title subtitle width height]
  (rl/rect! :x 0 :y 0 :width width :height height :color (rl/rgba 10 12 20 190))
  ;; measured text widths are often odd, and / on odd ints yields a ratio,
  ;; which DrawText's :int FFI arg rejects (invalid foreign-procedure
  ;; argument) — quot keeps every centre an int
  (let [centre-x (fn [text-w] (- (quot width 2) (quot text-w 2)))
        t-w (rl/text-width title :size 44)
        s-w (rl/text-width subtitle :size 20)]
    (rl/text! title :x (centre-x t-w) :y (- (quot height 2) 70)
              :size 44 :color rl/GOLD)
    (rl/text! subtitle :x (centre-x s-w) :y (- (quot height 2) 6)
              :size 20 :color rl/WHITE)))

(defn draw-frame!
  "Everything for one frame."
  [{:keys [world ui debris width height screen terrain]}]
  (rl/begin-drawing)
  (rl/clear-background (rl/rgba 120 175 225 255))
  (rl/with-camera-3d {:pos-x (CAMERA-POS 0) :pos-y (CAMERA-POS 1) :pos-z (CAMERA-POS 2)
                      :target-x (CAMERA-TARGET 0) :target-y (CAMERA-TARGET 1)
                      :target-z (CAMERA-TARGET 2)
                      :fovy 55.0 :projection 0}
    (fn []
      (draw-terrain! terrain)
      (when (= :game screen)
        (draw-trajectory! (:aim ui) (if (:charging ui)
                                      (min 1.0 (/ (:charge-t ui) input/CHARGE-TIME))
                                      0.55)))
      (doseq [b (:bodies world)]
        (draw-chunk! b))
      (when-let [b (:ball world)]
        (rl/sphere! :pos (or (:pos b) (:origin b)) :radius 0.5
                    :rings 10 :slices 14 :color rl/DARKBROWN))
      (draw-debris! debris)))
  (when (= :game screen)
    (draw-hud! world ui width height)
    (draw-crosshair! (:mx ui) (:my ui)))
  (when (= :title screen)
    (draw-overlay! "VOXEL SIEGE"
                   "click to start - 5 shots - topple 70% of the castle" width height))
  (when (= :end screen)
    (let [pct (int (* 100 (:destruction world)))]
      (if (= :won (:phase world))
        (draw-overlay! "CASTLE FELLED"
                       (str "destruction " pct "% - R or click to play again") width height)
        (draw-overlay! "OUT OF AMMO"
                       (str "destruction " pct "% - R or click to try again") width height))))
  (rl/fps! :x 8 :y 8)
  (rl/end-drawing))
