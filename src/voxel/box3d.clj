(ns voxel.box3d
  "Box3D physics (github.com/erincatto/box3d) over its C ABI via jolt.ffi.

  Box3D passes b3Vec3/b3Quat by value as float HFAs and ids as int composites,
  which jolt.ffi cannot express, so every call goes through the pointer/scalar
  shim in native/voxel_b3.c. World ids cross as :uint32, body ids as :uint64
  (the bit patterns of b3StoreWorldId / b3StoreBodyId). Body transforms are
  read through pre-allocated scratch buffers — the frame loop is single
  threaded, so reuse is safe."
  (:require [jolt.ffi :as ffi]))

(def ^:const STATIC-BODY 0)
(def ^:const DYNAMIC-BODY 2)

(ffi/defcfn world-create*   "vb3_world_create"      [:double :double :double :int] :uint32)
(ffi/defcfn world-destroy*  "vb3_world_destroy"     [:uint32] :void)
(ffi/defcfn world-step*     "vb3_world_step"        [:uint32 :float :int] :void)
(ffi/defcfn world-explode*  "vb3_world_explode"     [:uint32 :double :double :double :float :float :float] :void)
(ffi/defcfn body-create*    "vb3_body_create"       [:uint32 :int :double :double :double :double :double :double :double :int] :uint64)
(ffi/defcfn body-destroy*   "vb3_body_destroy"      [:uint64] :void)
(ffi/defcfn body-add-box*   "vb3_body_add_box"      [:uint64 :double :double :double :float :float :float :float :float :float] :void)
(ffi/defcfn ball-create*    "vb3_ball_create"       [:uint32 :double :double :double :float :float :float :float :float :float :float] :uint64)
(ffi/defcfn body-transform* "vb3_body_transform"    [:uint64 :pointer :pointer] :void)
(ffi/defcfn body-velocity*  "vb3_body_velocity"     [:uint64 :pointer] :void)
(ffi/defcfn body-awake*     "vb3_body_awake"        [:uint64] :int)
(ffi/defcfn body-set-vel*   "vb3_body_set_velocity" [:uint64 :float :float :float] :void)
(ffi/defcfn body-set-awake* "vb3_body_set_awake"    [:uint64 :int] :void)

;; scratch buffers reused by every transform/velocity read
(def ^:private pos-buf (ffi/alloc 24))
(def ^:private quat-buf (ffi/alloc 16))
(def ^:private vel-buf (ffi/alloc 12))

(defn create-world
  "Create a Box3D world with gravity [gx gy gz] and worker-count threads.
  Returns the world id (nonzero)."
  [gx gy gz worker-count]
  (world-create* (double gx) (double gy) (double gz) (int worker-count)))

(defn destroy-world! [world] (world-destroy* (int world)))

(defn step!
  "Advance the world by dt seconds using substeps sub-steps (4 is Box3D's
  recommended value)."
  [world dt substeps]
  (world-step* (int world) (double dt) (int substeps)))

(defn explode!
  "Apply a radial impulse at [x y z]. Falloff is the distance beyond radius
  over which the impulse decays to zero."
  [world x y z radius falloff impulse-per-area]
  (world-explode* (int world) (double x) (double y) (double z)
                  (double radius) (double falloff) (double impulse-per-area)))

(defn create-body
  "Create a body of type (voxel.box3d/STATIC-BODY or DYNAMIC-BODY) at [x y z]
  with initial rotation [qx qy qz qw]. Pass awake 0 to spawn it already
  sleeping (grounded masonry) — costs nothing until disturbed."
  [world type x y z qx qy qz qw awake]
  (body-create* (int world) (int type) (double x) (double y) (double z)
                (double qx) (double qy) (double qz) (double qw) (int awake)))

(defn destroy-body! [body] (body-destroy* body))

(defn add-box!
  "Attach a box shape with half-extents hx/hy/hz at local offset [lx ly lz]
  to a body, making it a compound of cells when called once per cell."
  [body lx ly lz hx hy hz density friction restitution]
  (body-add-box* body (double lx) (double ly) (double lz)
                 (double hx) (double hy) (double hz)
                 (double density) (double friction) (double restitution)))

(defn create-ball
  "Create a dynamic sphere at [x y z] with initial velocity [vx vy vz]."
  [world x y z radius vx vy vz density friction restitution]
  (ball-create* (int world) (double x) (double y) (double z) (double radius)
                (double vx) (double vy) (double vz)
                (double density) (double friction) (double restitution)))

(defn transform
  "Body transform as [[x y z] [qx qy qz qw]]."
  [body]
  (body-transform* body pos-buf quat-buf)
  [[(ffi/read pos-buf :double 0) (ffi/read pos-buf :double 8) (ffi/read pos-buf :double 16)]
   [(ffi/read quat-buf :float 0) (ffi/read quat-buf :float 4)
    (ffi/read quat-buf :float 8) (ffi/read quat-buf :float 12)]])

(defn velocity
  "Body linear velocity as [vx vy vz]."
  [body]
  (body-velocity* body vel-buf)
  [(ffi/read vel-buf :float 0) (ffi/read vel-buf :float 4) (ffi/read vel-buf :float 8)])

(defn awake? [body] (== 1 (body-awake* body)))

(defn set-velocity! [body vx vy vz]
  (body-set-vel* body (double vx) (double vy) (double vz)))

(defn set-awake! [body awake]
  (body-set-awake* body (int (if awake 1 0))))
