(ns voxel.main
  "Window setup + the frame loop: input -> world step -> render."
  (:require [voxel.raylib :as rl]
            [voxel.input :as input]
            [voxel.world :as w]
            [voxel.render :as render]))

(def WIDTH 960)
(def HEIGHT 540)
(def MAX-DEBRIS 240)

;; headless smoke: fire one scripted shot so a screenshot can capture impact
(def ^:private autofire-frame
  (when-let [v (System/getenv "VOXEL_APP_AUTOFIRE")]
    (try (Integer/parseInt v) (catch Exception _ nil))))

(defn- spawn-debris
  [debris events]
  (let [new (mapcat (fn [ev]
                      (let [[_ x y z n] ev
                            cnt (max 3 (min 12 (long (/ n 3))))]
                        (map (fn [_]
                               (let [a (/ (rl/get-random-value 0 628) 100.0)]
                                 {:x (double x) :y (double y) :z (double z)
                                  :vx (* 6.0 (Math/sin a))
                                  :vy (+ 4.0 (rl/get-random-value 0 60) 0.0)
                                  :vz (* 6.0 (Math/cos a))
                                  :size 0.3
                                  :color (rl/rgba 148 151 165 255)
                                  :ttl (+ 0.7 (/ (rl/get-random-value 0 100) 100.0))}))
                             (range cnt))))
                    events)]
    (into [] (take-last MAX-DEBRIS (into debris new)))))

(defn- step-debris
  [debris dt]
  (into []
        (comp
         (filter #(> (:ttl %) 0))
         (map (fn [d]
                (let [vy (- (:vy d) (* 12.0 dt))]
                  (-> d
                      (assoc :x (+ (:x d) (* (:vx d) dt))
                             :y (+ (:y d) (* vy dt))
                             :z (+ (:z d) (* (:vz d) dt))
                             :vy vy
                             :ttl (- (:ttl d) dt)))))))
        debris))

(defn- end-screen
  "Which screen are we on: :title at first, :end when the round is over."
  [screen world]
  (cond
    (= :title screen) :title
    (contains? #{:won :lost} (:phase world)) :end
    :else :game))

(defn -main
  [& _]
  (rl/window! :width WIDTH :height HEIGHT :title "voxel siege - divergence theorem sandbox")
  (rl/set-target-fps 60)
  (let [deadline (rl/auto-quit-deadline)
        summary (volatile! nil)]
    (loop [frame 0
           world (w/initial-state)
           ;; smoke mode (VOXEL_APP_AUTOFIRE) skips the title screen
           screen (if autofire-frame :game :title)
           ui {:charging false :charge-t 0.0 :aim [0.0 0.4] :mx (/ WIDTH 2) :my (/ HEIGHT 2)}
           debris []
           consumed 0]
      (if (rl/keep-running? deadline)
        (let [dt (double (rl/get-frame-time))
              in (input/snapshot WIDTH HEIGHT)
              ;; title/end: any click or R restarts the round
              restart-now (or (and (not= :game screen) (or (:pressed? in) (:restart? in)))
                              (and (= :game screen) (:restart? in)))
               world (if restart-now (w/initial-state) world)
               screen (if restart-now :game screen)
               consumed (if restart-now 0 consumed)
              ;; charging state machine (game screen only)
              can-charge? (and (= :game screen) (nil? (:ball world))
                               (pos? (:balls-left world))
                               (= :playing (:phase world)))
              charging (cond
                         (and can-charge? (:pressed? in)) true
                         (not (:down? in)) false
                         :else (:charging ui))
              charge-t (cond
                         (not charging) 0.0
                         (:pressed? in) 0.0
                         :else (+ (:charge-t ui) dt))
               ;; scripted smoke-test shot fires all by itself
               autofire? (and (= frame autofire-frame) (= :game screen) can-charge?)
               ;; on the release frame `charging` is already false and `charge-t`
               ;; already reset, so the shot must read BOTH from last frame's ui
               ;; (:pressed? also covers a tap released within one frame)
               fire-now (or (and (or (:charging ui) (:pressed? in))
                                 (:released? in)
                                 can-charge?)
                            autofire?)
               power (if autofire?
                       0.9
                       (max 0.15 (input/power-from-charge (:charge-t ui))))
              aim (if autofire? [0.0 0.40] (:aim in))
              world (if fire-now
                      (w/fire world (apply w/dir-from-yaw-pitch aim) power)
                      world)
               ;; simulate + visual debris from events
               world' (w/step world dt)
               ;; :events is an ever-growing log — spawn only the entries this
               ;; frame added, or every past hit re-spawns particles forever
               fresh-events (drop consumed (:events world'))
               debris' (-> debris
                           (spawn-debris fresh-events)
                           (step-debris dt))
               consumed' (count (:events world'))
              ui (assoc ui
                        :charging charging
                        :charge-t charge-t
                        :aim (:aim in)
                        :mx (:mx in) :my (:my in))]
          (render/draw-frame! {:world world' :ui ui :debris debris'
                               :width WIDTH :height HEIGHT
                               :screen (end-screen screen world')})
           (rl/maybe-screenshot! frame 150)
            (vreset! summary {:frame frame
                              :destruction (:destruction world')
                              :events (mapv first (:events world'))
                              :debris (count debris')
                              :phase (:phase world')})
            (recur (inc frame) world' screen ui debris' consumed'))
        (when autofire-frame
          (println "[voxel] smoke summary:" (pr-str @summary))))))
  (rl/close-window))
