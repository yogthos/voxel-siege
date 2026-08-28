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
  [{:keys [world ui debris width height screen]}]
  (rl/begin-drawing)
  (rl/clear-background (rl/rgba 120 175 225 255))
  (rl/with-camera-3d {:pos-x (CAMERA-POS 0) :pos-y (CAMERA-POS 1) :pos-z (CAMERA-POS 2)
                      :target-x (CAMERA-TARGET 0) :target-y (CAMERA-TARGET 1)
                      :target-z (CAMERA-TARGET 2)
                      :fovy 55.0 :projection 0}
    (fn []
      (rl/draw-grid 70 1.0)
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
