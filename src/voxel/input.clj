(ns voxel.input
  "Raylib input reads ONLY. Reports raw intents: mouse position, aim
  yaw/pitch, button edges, restart key. Owns no game state."
  (:require [voxel.raylib :as rl]))

(def YAW-MAX 0.35)      ; radians either side of straight downrange
(def PITCH-MIN 0.04)    ; near-flat shots
(def PITCH-MAX 0.85)    ; high lobs
(def CHARGE-TIME 1.1)   ; seconds of hold from zero to full power

(defn aim-from-mouse
  "Mouse position -> [yaw pitch]. Screen right aims right (+x), screen top
  aims higher."
  [mx my width height]
  [(double (* (- (/ (double mx) width) 0.5) 2.0 YAW-MAX))
   (-> (- 1.0 (/ (double my) height))
       (* PITCH-MAX)
       (max PITCH-MIN)
       (min PITCH-MAX))])

(defn power-from-charge
  "Charge seconds in [0, CHARGE-TIME] -> power in [0,1]."
  [charge-t]
  (max 0.0 (min 1.0 (/ (double charge-t) CHARGE-TIME))))

(defn snapshot
  "One frame of input as pure data."
  [width height]
  {:mx (rl/get-mouse-x)
   :my (rl/get-mouse-y)
   :aim (aim-from-mouse (rl/get-mouse-x) (rl/get-mouse-y) width height)
   :pressed? (rl/mouse-pressed? rl/MOUSE-LEFT)
   :down? (rl/mouse-down? rl/MOUSE-LEFT)
   :released? (rl/mouse-released? rl/MOUSE-LEFT)
   :restart? (rl/key-pressed? rl/KEY-R)})
