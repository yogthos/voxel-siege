# voxel-siege

An Angry-Birds-style artillery sandbox in [Jolt](https://github.com/jolt-lang/jolt)
(Clojure on Chez Scheme), rendered with raylib over the C ABI.

You command a cannon at the near end of a range. A voxel castle stands at the
far end. Hold the left mouse button to charge, release to fire. Five shots per
round; topple 70% of the castle to win.

The interesting part: destruction scoring, chunk mass, and center-of-mass all
come from the divergence theorem with volumes and centroids being computed from
the surface mesh.

## Run

```
jolt -M:run          # needs a display (or see headless below)
```

- Aim: move the mouse (yaw/pitch of the barrel)
- Fire: hold LMB to charge, release to fire (longer hold = faster shot)
- R: restart the round

## Test

```
jolt -M:test         # 23 tests, 136 assertions
```

All game logic (`voxel.mesh`, `voxel.world`) is pure with no raylib calls or
window making it fully testable in a terminal.

## Headless smoke

```
RAYLIB_APP_AUTO_QUIT_MS=4000 RAYLIB_APP_SHOT=vs_title.png jolt -M:run
VOXEL_APP_AUTOFIRE=40 RAYLIB_APP_AUTO_QUIT_MS=10000 \
  RAYLIB_APP_SHOT=vs_impact.png jolt -M:run
```

`RAYLIB_APP_SHOT` must be a *bare filename* (raylib prepends the cwd).
`VOXEL_APP_AUTOFIRE=<frame>` skips the title screen and fires one scripted
shot (pitch 0.40, power 0.9); on exit it prints a summary line:

```
[voxel] smoke summary: {:frame 262, :destruction 0.089..., :events [:blast :shatter :shatter], :phase :playing}
```

## Voxel computation

For a closed triangulated surface, the volume is

```
V = (1/6) Σ  (Δ₁ × Δ₂)ₓ · (x₀ + x₁ + x₂)
```

Each voxel occupies `[i,i+1]×[j,j+1]×[k,k+1]` (volume exactly 1), so the test
is the mesh volume of any voxel set equals its voxel count, and
the centroid equals the mean cell center. That identity is asserted across
boxes, walls, hollow shells, L-shapes, blobs, and random clusters.


- **Destruction %** = `1 − mesh-volume(remaining)/mesh-volume(initial)`,
  recomputed from the surface mesh after every blast rathet than a cell count.
- **Chunk mass** = `mesh-volume` of the island's surface at detach.
- **Chunk center of mass** = `mesh-centroid` of that surface.

