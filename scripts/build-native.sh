#!/bin/sh
# Builds vendored Box3D (if needed) and the vb3 FFI shim dylib.
set -e
cd "$(dirname "$0")/.."

if [ ! -f vendor/box3d/build/bin/libbox3d.dylib ]; then
    if [ ! -d vendor/box3d ]; then
        git clone --depth 1 https://github.com/erincatto/box3d.git vendor/box3d
    fi
    cmake -S vendor/box3d -B vendor/box3d/build -DCMAKE_BUILD_TYPE=Release \
          -DBUILD_SHARED_LIBS=ON -DBOX3D_SAMPLES=OFF -DBOX3D_UNIT_TESTS=OFF \
          -DBOX3D_BENCHMARKS=OFF -DBOX3D_DOCS=OFF
    cmake --build vendor/box3d/build -j8
fi

mkdir -p native
cc -O2 -std=c11 -fPIC -shared \
   -Ivendor/box3d/include native/voxel_b3.c \
   -Lvendor/box3d/build/bin -lbox3d \
   -Wl,-rpath,@loader_path/../vendor/box3d/build/bin \
   -o native/libvoxel_b3.dylib
echo "built: native/libvoxel_b3.dylib"
