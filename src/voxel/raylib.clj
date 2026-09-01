(ns voxel.raylib
  "Vendored subset of the raylib FFI binding layer"
  (:require
   [jolt.ffi :as ffi]))

;; --- Color -------------------------------------------------------------------
(defn rgba
  "Pack an RGBA color into the little-endian uint32 that raylib's `Color` struct
  is (r | g<<8 | b<<16 | a<<24), so it can cross the FFI boundary as a :uint."
  [r g b a]
  (bit-or (int r) (bit-shift-left (int g) 8)
          (bit-shift-left (int b) 16) (bit-shift-left (int a) 24)))

(def LIGHTGRAY (rgba 200 200 200 255))   (def GRAY       (rgba 130 130 130 255))
(def DARKGRAY  (rgba 80 80 80 255))      (def YELLOW     (rgba 253 249 0 255))
(def GOLD      (rgba 255 203 0 255))     (def ORANGE     (rgba 255 161 0 255))
(def PINK      (rgba 255 109 194 255))   (def RED        (rgba 230 41 55 255))
(def MAROON    (rgba 190 33 55 255))     (def GREEN      (rgba 0 228 48 255))
(def LIME      (rgba 0 158 47 255))      (def DARKGREEN  (rgba 0 117 44 255))
(def SKYBLUE   (rgba 102 191 255 255))   (def BLUE       (rgba 0 121 241 255))
(def DARKBLUE  (rgba 0 82 172 255))      (def PURPLE     (rgba 200 122 255 255))
(def VIOLET    (rgba 135 60 190 255))    (def DARKPURPLE (rgba 112 31 126 255))
(def BEIGE     (rgba 211 176 131 255))   (def BROWN      (rgba 127 106 79 255))
(def DARKBROWN (rgba 76 63 47 255))      (def WHITE      (rgba 255 255 255 255))
(def BLACK     (rgba 0 0 0 255))         (def MAGENTA    (rgba 255 0 255 255))
(def RAYWHITE  (rgba 245 245 245 255))

;; --- window / lifecycle ------------------------------------------------------
(ffi/defcfn init-window    "InitWindow"   [:int :int :string] :void)
(ffi/defcfn set-target-fps "SetTargetFPS" [:int] :void)
(ffi/defcfn close-window   "CloseWindow"  [] :void)
(ffi/defcfn ^:private should-close-raw "WindowShouldClose" [] :int)

;; --- frame -------------------------------------------------------------------
(ffi/defcfn begin-drawing    "BeginDrawing"    [] :void)
(ffi/defcfn end-drawing      "EndDrawing"      [] :void)
(ffi/defcfn clear-background "ClearBackground" [:uint] :void)
(ffi/defcfn get-frame-time   "GetFrameTime"    [] :float) ; seconds since last frame

;; --- 2D text + rects (scalar variants; Color is the only by-value struct) ----
(ffi/defcfn draw-text            "DrawText"            [:string :int :int :int :uint] :void)
(ffi/defcfn draw-fps             "DrawFPS"             [:int :int] :void)
(ffi/defcfn measure-text         "MeasureText"         [:string :int] :int)
(ffi/defcfn draw-line            "DrawLine"            [:int :int :int :int :uint] :void)
(ffi/defcfn draw-rectangle       "DrawRectangle"       [:int :int :int :int :uint] :void)
(ffi/defcfn draw-rectangle-lines "DrawRectangleLines"  [:int :int :int :int :uint] :void)
(ffi/defcfn draw-circle          "DrawCircle"          [:int :int :float :uint] :void)

;; --- rlgl immediate mode (all scalar), for 3D triangles / lines --------------
(ffi/defcfn rl-begin     "rlBegin"     [:int] :void)   ; RL-LINES / RL-TRIANGLES
(ffi/defcfn rl-end       "rlEnd"       [] :void)
(ffi/defcfn rl-vertex-2f "rlVertex2f"  [:float :float] :void)
(ffi/defcfn rl-vertex-3f "rlVertex3f"  [:float :float :float] :void)
(ffi/defcfn rl-color-4ub "rlColor4ub"  [:int :int :int :int] :void)  ; u8 args
(def ^:const RL-LINES 1)
(def ^:const RL-TRIANGLES 4)

(defn rl-color!
  "rlColor4ub from a packed rgba Color, so rlgl immediate mode can use the same
  Color values as the rest of the API."
  [color]
  (rl-color-4ub (bit-and color 0xff)
                (bit-and (bit-shift-right color 8) 0xff)
                (bit-and (bit-shift-right color 16) 0xff)
                (bit-and (bit-shift-right color 24) 0xff)))

;; --- rlgl matrix stack (rotated chunk drawing) --------------------------------
(ffi/defcfn rl-push-matrix "rlPushMatrix"  [] :void)
(ffi/defcfn rl-pop-matrix  "rlPopMatrix"   [] :void)
(ffi/defcfn rl-translate-f "rlTranslatef"  [:float :float :float] :void)
(ffi/defcfn rl-rotate-f    "rlRotatef"     [:float :float :float :float] :void)

(defn rl-rotate-quaternion!
  "Apply quaternion [qx qy qz qw] to the current matrix via rlRotatef
  (axis-angle form; near-identity quaternions are skipped)."
  [[qx qy qz qw]]
  (let [w (min 1.0 (max -1.0 (double qw)))
        angle (* 2.0 (Math/acos w))
        s (Math/sin (* 0.5 angle))]
    (when (> (Math/abs s) 1e-6)
      (rl-rotate-f (Math/toDegrees angle)
                   (double (/ qx s)) (double (/ qy s)) (double (/ qz s))))))

;; --- Camera3D + 3D geometry --------------------------------------------------
;; Camera3D is 44 bytes (three Vector3 + a float + an int), passed BY VALUE to
;; BeginMode3D, the >16-byte-struct-by-pointer approach. 3D shape helpers like
;; DrawCube take a Vector3 BY VALUE, so draw 3D geometry with rlgl immediate
;; mode instead. DrawGrid is scalar.
(ffi/defcfn draw-grid    "DrawGrid"    [:int :float] :void)
(ffi/defcfn ^:private begin-mode-3d-ptr "BeginMode3D" [:pointer] :void)
(ffi/defcfn end-mode-3d "EndMode3D" [] :void)

(defn with-camera-3d
  "Run (f) with a Camera3D active (BeginMode3D → f → EndMode3D). Builds the
  44-byte struct in native memory (nine floats + fovy + projection int) and passes
  a pointer. Keys: :pos-x/y/z :target-x/y/z :up-x/y/z :fovy :projection (0 =
  perspective)."
  [{:keys [pos-x pos-y pos-z target-x target-y target-z up-x up-y up-z fovy projection]
    :or {pos-x 0
         pos-y 0
         pos-z 0
         target-x 0
         target-y 0
         target-z 0
         up-x 0
         up-y 1
         up-z 0
         fovy 45
         projection 0}} f]
  (let [p (ffi/alloc 44)]
    (try
      (ffi/write p :float (double pos-x) 0)
      (ffi/write p :float (double pos-y) 4)
      (ffi/write p :float (double pos-z) 8)
      (ffi/write p :float (double target-x) 12)
      (ffi/write p :float (double target-y) 16)
      (ffi/write p :float (double target-z) 20)
      (ffi/write p :float (double up-x) 24)
      (ffi/write p :float (double up-y) 28)
      (ffi/write p :float (double up-z) 32)
      (ffi/write p :float (double fovy) 36)
      (ffi/write p :int (int projection) 40)
      (begin-mode-3d-ptr p)
      (f)
      (end-mode-3d)
      (finally (ffi/free p)))))

(defn shade
  "Darken a packed Color by factor f (fakes lighting so faces read as 3D)."
  [color f]
  (rgba (int (* f (bit-and color 0xff)))
        (int (* f (bit-and (bit-shift-right color 8) 0xff)))
        (int (* f (bit-and (bit-shift-right color 16) 0xff)))
        255))

(defn quad!
  "Two rlgl triangles for a quad, given a shaded color and a vector of its four
  [x y z] corners in a→b→c→d winding order."
  [color [a b c d]]
  (rl-color! color)
  (let [[ax ay az] a [bx by bz] b [cx cy cz] c [dx dy dz] d]
    (rl-vertex-3f ax ay az) (rl-vertex-3f bx by bz) (rl-vertex-3f cx cy cz)
    (rl-vertex-3f ax ay az) (rl-vertex-3f cx cy cz) (rl-vertex-3f dx dy dz)))

(defn cube!
  "Draw an axis-aligned box via rlgl immediate mode, its faces shaded from the
  packed `:color` for depth. Must be called inside a BeginMode3D block (see
  with-camera-3d). Keyword args:
    :pos   [x y z] centre           (default [0 0 0])
    :size  a number for a uniform cube, or [sx sy sz]  (default 1)
    :color a packed Color           (default BLACK)"
  [& {:keys [pos size color]
      :or {pos [0.0 0.0 0.0]
           size 1.0
           color BLACK}}]
  (let [[cx cy cz] pos
        [sx sy sz] (if (number? size) [size size size] size)
        hx (/ sx 2.0) hy (/ sy 2.0) hz (/ sz 2.0)
        x0 (- cx hx) x1 (+ cx hx) y0 (- cy hy) y1 (+ cy hy) z0 (- cz hz) z1 (+ cz hz)
        ;; the eight corners, named a<x><y><z> by which extreme each axis takes
        a000 [x0 y0 z0] a100 [x1 y0 z0] a010 [x0 y1 z0] a110 [x1 y1 z0]
        a001 [x0 y0 z1] a101 [x1 y0 z1] a011 [x0 y1 z1] a111 [x1 y1 z1]]
    (rl-begin RL-TRIANGLES)
    (quad! (shade color 1.0)  [a001 a101 a111 a011])   ; front  +z
    (quad! (shade color 0.5)  [a100 a000 a010 a110])   ; back   -z
    (quad! (shade color 0.7)  [a000 a001 a011 a010])   ; left   -x
    (quad! (shade color 0.85) [a101 a100 a110 a111])   ; right  +x
    (quad! (shade color 1.0)  [a011 a111 a110 a010])   ; top    +y
    (quad! (shade color 0.4)  [a000 a100 a101 a001])   ; bottom -y
    (rl-end)))

(defn sphere!
  "Draw a sphere via rlgl immediate mode (lat/long tessellation), faces shaded
  from the packed `:color` for depth (brighter toward +y). Must be called inside
  a BeginMode3D block (see with-camera-3d). Keyword args:
    :pos    [x y z] centre        (default [0 0 0])
    :radius a number              (default 0.5)
    :rings  latitude bands        (default 12)
    :slices longitude sectors     (default 16)
    :color  a packed Color        (default BLACK)"
  [& {:keys [pos radius rings slices color]
      :or {pos [0.0 0.0 0.0]
           radius 0.5
           rings 12
           slices 16
           color BLACK}}]
  (let [[cx cy cz] pos
        two-pi (* 2.0 Math/PI)]
    (rl-begin RL-TRIANGLES)
    (dotimes [i rings]
      (let [lat0 (- (* Math/PI (/ (double i) rings)) (/ Math/PI 2.0))
            lat1 (- (* Math/PI (/ (double (inc i)) rings)) (/ Math/PI 2.0))
            y0 (Math/sin lat0) y1 (Math/sin lat1)
            r0 (Math/cos lat0) r1 (Math/cos lat1)
            brightness (+ 0.45 (* 0.55 (/ (+ y0 y1 2.0) 4.0)))
            shaded (shade color brightness)]
        (dotimes [j slices]
          (let [lon0 (* two-pi (/ (double j) slices))
                lon1 (* two-pi (/ (double (inc j)) slices))
                s0 (Math/sin lon0) c0 (Math/cos lon0)
                s1 (Math/sin lon1) c1 (Math/cos lon1)
                p00 [(+ cx (* radius r0 c0)) (+ cy (* radius y0)) (+ cz (* radius r0 s0))]
                p01 [(+ cx (* radius r0 c1)) (+ cy (* radius y0)) (+ cz (* radius r0 s1))]
                p10 [(+ cx (* radius r1 c0)) (+ cy (* radius y1)) (+ cz (* radius r1 s0))]
                p11 [(+ cx (* radius r1 c1)) (+ cy (* radius y1)) (+ cz (* radius r1 s1))]]
            (quad! shaded [p00 p10 p11 p01])))))
    (rl-end)))

(defn frustum!
  "Draw a vertical frustum (truncated cone) via rlgl triangles: one
  flat-shaded quad per radial segment plus an optional top cap, faking
  lighting like cube! does. Must be called inside a BeginMode3D block.
  Keyword args:
    :pos      [x y z] base centre   (default [0 0 0])
    :base-r   radius at the base    (default 1)
    :top-r    radius at the top     (default 0 = cone)
    :height   vertical extent       (default 1)
    :segments radial slices         (default 8)
    :color    packed Color          (default BLACK)
    :cap?     draw the top disc     (default true)"
  [& {:keys [pos base-r top-r height segments color cap?]
      :or {pos [0.0 0.0 0.0]
           base-r 1.0
           top-r 0.0
           height 1.0
           segments 8
           color BLACK
           cap? true}}]
  (let [[cx cy cz] pos
        two-pi (* 2.0 Math/PI)]
    (rl-begin RL-TRIANGLES)
    (dotimes [j segments]
      (let [lon0 (* two-pi (/ (double j) segments))
            lon1 (* two-pi (/ (double (inc j)) segments))
            s0 (Math/sin lon0) c0 (Math/cos lon0)
            s1 (Math/sin lon1) c1 (Math/cos lon1)
            b0 [(- cx (* base-r s0)) cy (- cz (* base-r c0))]
            b1 [(- cx (* base-r s1)) cy (- cz (* base-r c1))]
            t0 [(- cx (* top-r s0)) (+ cy height) (- cz (* top-r c0))]
            t1 [(- cx (* top-r s1)) (+ cy height) (- cz (* top-r c1))]
            ;; flat per-face light: brightest facing the camera (+z)
            mid (* 0.5 (+ lon0 lon1))
            f (+ 0.72 (* 0.28 (- 1.0 (* 0.5 (+ 1.0 (Math/cos mid))))))]
        (quad! (shade color f) [b0 b1 t1 t0])
        (when cap?
          (rl-color! (shade color 1.0))
          (let [tc [cx (+ cy height) cz]]
            (rl-vertex-3f (t0 0) (t0 1) (t0 2))
            (rl-vertex-3f (t1 0) (t1 1) (t1 2))
            (rl-vertex-3f (tc 0) (tc 1) (tc 2))))))
    (rl-end)))

;; --- input -------------------------------------------------------------------
(ffi/defcfn ^:private key-down-raw     "IsKeyDown"          [:int] :int)
(ffi/defcfn ^:private key-pressed-raw  "IsKeyPressed"       [:int] :int)
(ffi/defcfn ^:private mouse-down-raw   "IsMouseButtonDown"  [:int] :int)
(ffi/defcfn ^:private mouse-pressed-raw "IsMouseButtonPressed" [:int] :int)
(ffi/defcfn ^:private mouse-released-raw "IsMouseButtonReleased" [:int] :int)
(ffi/defcfn get-mouse-x      "GetMouseX"         [] :int)
(ffi/defcfn get-mouse-y      "GetMouseY"         [] :int)
(ffi/defcfn get-random-value "GetRandomValue"    [:int :int] :int)

;; --- screenshot hook plumbing (headless smoke tests) -------------------------
(ffi/defcfn take-screenshot       "TakeScreenshot"          [:string] :void)
(ffi/defcfn ^:private flush-batch "rlDrawRenderBatchActive" [] :void)

;; C-bool returns arrive in the low byte; mask so only 0/1 counts.
(defn window-should-close?
  []
  (not (zero? (bit-and (should-close-raw) 0xff))))

(defn key-down?
  [k]
  (not (zero? (bit-and (key-down-raw k) 0xff))))

(defn key-pressed?
  [k]
  (not (zero? (bit-and (key-pressed-raw k) 0xff))))

(defn mouse-down?
  [b]
  (not (zero? (bit-and (mouse-down-raw b) 0xff))))

(defn mouse-pressed?
  [b]
  (not (zero? (bit-and (mouse-pressed-raw b) 0xff))))

(defn mouse-released?
  [b]
  (not (zero? (bit-and (mouse-released-raw b) 0xff))))

;; --- constants (raylib KeyboardKey / MouseButton) ----------------------------
(def ^:const KEY-SPACE 32)  (def ^:const KEY-R     82)
(def ^:const KEY-ESCAPE 256) (def ^:const KEY-ENTER 257)
(def ^:const MOUSE-LEFT 0)
(def ^:const MOUSE-RIGHT 1)

;; --- ergonomic keyword-argument drawing API ----------------------------------
(defn window!
  "InitWindow with keyword args. :width :height :title."
  [& {:keys [width height title]
      :or {width 800
           height 450
           title "raylib"}}]
  (init-window width height title))

(defn text!
  "DrawText. :x :y :size :color."
  [s & {:keys [x y size color]
        :or {x 0
             y 0
             size 20
             color BLACK}}]
  (draw-text s x y size color))

(defn text-width
  "MeasureText. :size."
  [s & {:keys [size]
        :or {size 20}}]
  (measure-text s size))

(defn fps!
  "DrawFPS. :x :y."
  [& {:keys [x y]
      :or {x 10
           y 10}}]
  (draw-fps x y))

(defn rect!
  "DrawRectangle. :x :y :width :height :color."
  [& {:keys [x y width height color]
      :or {x 0
           y 0
           width 10
           height 10
           color BLACK}}]
  (draw-rectangle x y width height color))

(defn rect-lines!
  "DrawRectangleLines. :x :y :width :height :color."
  [& {:keys [x y width height color]
      :or {x 0
           y 0
           width 10
           height 10
           color BLACK}}]
  (draw-rectangle-lines x y width height color))

(defn circle!
  "DrawCircle. :x :y :radius :color."
  [& {:keys [x y radius color]
      :or {x 0
           y 0
           radius 10
           color BLACK}}]
  (draw-circle x y (double radius) color))

(defn line!
  "DrawLine. :x1 :y1 :x2 :y2 :color."
  [& {:keys [x1 y1 x2 y2 color]
      :or {x1 0
           y1 0
           x2 0
           y2 0
           color BLACK}}]
  (draw-line x1 y1 x2 y2 color))

;; --- smoke-test loop guards --------------------------------------------------
(defn auto-quit-deadline
  "RAYLIB_APP_AUTO_QUIT_MS=<n> ends the loop after n ms, so a window example is
  smoke-testable with no person at the keyboard. Returns an absolute ms deadline
  or nil."
  []
  (when-let [v (System/getenv "RAYLIB_APP_AUTO_QUIT_MS")]
    (try (let [ms (Integer/parseInt v)]
           (when (pos? ms) (+ (System/currentTimeMillis) ms)))
         (catch Exception _ nil))))

(defn keep-running?
  "True while the window is open and any RAYLIB_APP_AUTO_QUIT_MS deadline is unmet."
  [deadline]
  (and (not (window-should-close?))
       (or (nil? deadline) (< (System/currentTimeMillis) deadline))))

(def ^:private shot-path (System/getenv "RAYLIB_APP_SHOT"))

(defn maybe-screenshot!
  "RAYLIB_APP_SHOT=/path dumps one PNG on frame `at`, headless visual proof a
  frame rendered. Flushes raylib's batched geometry first (DrawText etc. is
  deferred until EndDrawing, so a mid-frame TakeScreenshot would miss it)."
  [frame at]
  (when (and shot-path (= frame at))
    (flush-batch)
    (take-screenshot shot-path)))
