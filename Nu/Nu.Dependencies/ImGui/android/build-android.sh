#!/usr/bin/env bash
# based on https://github.com/yCatDev/ImGui.NET-nativebuild/blob/5c0ddd633feac6c7bca3bce89f7aeaefda2465e7/build_android.sh
# and modified to include all modules in a single combined library, and to be runnable on Windows (Git Bash) without modification.
# copy this into the root directory of https://github.com/JoeTwizzle/ImGui.NET-nativebuild
set -euo pipefail

# --------------------------
# Config (override via env)
# --------------------------
: "${BUILD_FLAVORS:=Release Debug}"
: "${ANDROID_NDK:=${ANDROID_NDK:-}}"
: "${ANDROID_API:=24}"
: "${ABIS:=arm64-v8a x86_64}"
: "${JOBS:=0}"                     # 0 = auto
: "${CLEAN:=0}"                    # 1 = wipe _deps/_out
: "${FREETYPE_VERSION:=2.14.1}"
: "${FREETYPE_URL_XZ:=https://download.savannah.gnu.org/releases/freetype/freetype-${FREETYPE_VERSION}.tar.xz}"
: "${FREETYPE_URL_GZ:=https://download.savannah.gnu.org/releases/freetype/freetype-${FREETYPE_VERSION}.tar.gz}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CIMGUI_DIR="$SCRIPT_DIR"

if [[ ! -f "$CIMGUI_DIR/cimgui/cimgui.cpp" ]]; then
  echo "ERROR: Run this script from cimgui repo root (cimgui/cimgui.cpp not found)."
  exit 1
fi

need_cmd() {
  command -v "$1" >/dev/null 2>&1 || { echo "ERROR: missing '$1'"; exit 1; }
}

need_cmd cmake
need_cmd tar
if command -v ninja >/dev/null 2>&1; then
  CMAKE_GEN=(-G Ninja)
elif command -v make >/dev/null 2>&1; then
  # On Windows Git Bash, defaulting to Visual Studio can fail for Android cross builds.
  CMAKE_GEN=(-G "Unix Makefiles")
else
  CMAKE_GEN=()
fi

if command -v curl >/dev/null 2>&1; then
  # Windows Git Bash can fail certificate revocation checks in some environments.
  if [[ "${OS:-}" == "Windows_NT" ]]; then
    DL="curl -L --fail --retry 5 --retry-delay 2 --retry-all-errors --ssl-no-revoke -o"
  else
    DL="curl -L --fail --retry 5 --retry-delay 2 --retry-all-errors -o"
  fi
elif command -v wget >/dev/null 2>&1; then
  DL="wget -O"
else
  echo "ERROR: need curl or wget"
  exit 1
fi

if [[ -z "$ANDROID_NDK" ]]; then
  # Try common locations
  for p in \
    "/c/AndroidNDK" \
    "C:/AndroidNDK" \
    "$HOME/Android/Sdk/ndk-bundle" \
    "$HOME/Android/Sdk/ndk"/* \
    "/opt/android-ndk" \
    "/opt/android-ndk-r"* \
    ; do
    if [[ -f "$p/build/cmake/android.toolchain.cmake" ]]; then
      ANDROID_NDK="$p"
      break
    fi
  done
fi

TOOLCHAIN="$ANDROID_NDK/build/cmake/android.toolchain.cmake"
if [[ -z "$ANDROID_NDK" || ! -f "$TOOLCHAIN" ]]; then
  echo "ERROR: ANDROID_NDK not set or invalid. Expected: <NDK>/build/cmake/android.toolchain.cmake"
  exit 1
fi

if [[ "$JOBS" == "0" ]]; then
  if command -v nproc >/dev/null 2>&1; then JOBS="$(nproc)"; else JOBS="8"; fi
fi

DEPS_DIR="$CIMGUI_DIR/_deps_android"
OUT_DIR="$CIMGUI_DIR/_out_android"

FT_TARBALL_XZ="$DEPS_DIR/freetype-${FREETYPE_VERSION}.tar.xz"
FT_TARBALL_GZ="$DEPS_DIR/freetype-${FREETYPE_VERSION}.tar.gz"
FT_SRC_DIR="$DEPS_DIR/freetype-${FREETYPE_VERSION}"
FT_BUILD_ROOT="$DEPS_DIR/freetype/build"
FT_INSTALL_ROOT="$DEPS_DIR/freetype/install"

# We build a single combined Android shared library from all available modules.
ANDROID_CMAKE_ROOT="$OUT_DIR/cimgui/android_cmake"
CIMGUI_BUILD_ROOT="$OUT_DIR/cimgui/build"
CIMGUI_OUT_LIBROOT="$OUT_DIR/cimgui"   # final output: _out_android/cimgui/<ABI>/<BuildType>/libcimgui.so

if [[ "$CLEAN" == "1" ]]; then
  rm -rf "$DEPS_DIR" "$OUT_DIR"
fi

mkdir -p "$DEPS_DIR" "$OUT_DIR"

download_freetype() {
  mkdir -p "$DEPS_DIR"
  if command -v xz >/dev/null 2>&1; then
    if [[ ! -f "$FT_TARBALL_XZ" ]]; then
      echo "Downloading FreeType ${FREETYPE_VERSION} (tar.xz)..."
      $DL "$FT_TARBALL_XZ" "$FREETYPE_URL_XZ"
    fi
    if [[ ! -d "$FT_SRC_DIR" ]]; then
      echo "Extracting FreeType..."
      tar -xf "$FT_TARBALL_XZ" -C "$DEPS_DIR"
    fi
  else
    if [[ ! -f "$FT_TARBALL_GZ" ]]; then
      echo "xz not found; downloading FreeType ${FREETYPE_VERSION} (tar.gz)..."
      $DL "$FT_TARBALL_GZ" "$FREETYPE_URL_GZ"
    fi
    if [[ ! -d "$FT_SRC_DIR" ]]; then
      echo "Extracting FreeType..."
      tar -xf "$FT_TARBALL_GZ" -C "$DEPS_DIR"
    fi
  fi

  if [[ ! -f "$FT_SRC_DIR/CMakeLists.txt" ]]; then
    echo "ERROR: FreeType sources not extracted correctly: $FT_SRC_DIR"
    exit 1
  fi
}

build_freetype_one_abi() {
  local ABI="$1"
  local BT="${2:?BuildType missing (Release/Debug)}"
  local BDIR="$FT_BUILD_ROOT/$ABI/$BT"
  local IDIR="$FT_INSTALL_ROOT/$ABI"

  rm -rf "$BDIR"
  mkdir -p "$BDIR" "$IDIR"

  echo "=== Building FreeType for $ABI ==="
  cmake -S "$FT_SRC_DIR" -B "$BDIR" "${CMAKE_GEN[@]}" \
    -DCMAKE_TOOLCHAIN_FILE="$TOOLCHAIN" \
    -DANDROID_ABI="$ABI" \
    -DANDROID_PLATFORM="android-$ANDROID_API" \
    -DCMAKE_BUILD_TYPE="$BT" \
    -DCMAKE_INSTALL_PREFIX="$IDIR" \
    -DBUILD_SHARED_LIBS=OFF \
    -DFT_DISABLE_ZLIB=TRUE \
    -DFT_DISABLE_BZIP2=TRUE \
    -DFT_DISABLE_PNG=TRUE \
    -DFT_DISABLE_HARFBUZZ=TRUE \
    -DFT_DISABLE_BROTLI=TRUE

  cmake --build "$BDIR" --config "$BT" -- -j "$JOBS"
  cmake --install "$BDIR"
}

generate_android_cmakelists() {
  mkdir -p "$ANDROID_CMAKE_ROOT"
  cat > "$ANDROID_CMAKE_ROOT/CMakeLists.txt" <<'CMAKE'
cmake_minimum_required(VERSION 3.10)
project(cimgui_android C CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Path to cimgui repository root must be provided by the build script
if(NOT DEFINED CIMGUI_ROOT)
  message(FATAL_ERROR "CIMGUI_ROOT is not set. Pass -DCIMGUI_ROOT=/path/to/cimgui")
endif()

# freetype config dir is provided by build script (-Dfreetype_DIR=...)
find_package(freetype REQUIRED CONFIG)

# Common ImGui sources used by all modules.
set(IMGUI_COMMON_SOURCES
  ${CIMGUI_ROOT}/cimgui/imgui/imgui.cpp
  ${CIMGUI_ROOT}/cimgui/imgui/imgui_draw.cpp
  ${CIMGUI_ROOT}/cimgui/imgui/imgui_demo.cpp
  ${CIMGUI_ROOT}/cimgui/imgui/imgui_widgets.cpp
  ${CIMGUI_ROOT}/cimgui/imgui/imgui_tables.cpp
)

set(COMBINED_SOURCES
  ${CIMGUI_ROOT}/cimgui/cimgui.cpp
  ${IMGUI_COMMON_SOURCES}
  ${CIMGUI_ROOT}/cimgui/imgui/misc/freetype/imgui_freetype.cpp
)

set(COMBINED_INCLUDES
  ${CIMGUI_ROOT}/cimgui
  ${CIMGUI_ROOT}/cimgui/imgui
)

set(COMBINED_DEFS
  IMGUI_DEFINE_MATH_OPERATORS=1
  IMGUI_ENABLE_FREETYPE=1
  IMGUI_ENABLE_STB_TRUETYPE=1
  IMGUI_DISABLE_OBSOLETE_FUNCTIONS=1
  IMGUI_USE_WCHAR32=1
)

if(EXISTS "${CIMGUI_ROOT}/cimplot/cimplot.cpp" AND EXISTS "${CIMGUI_ROOT}/cimplot/implot/implot.cpp")
  list(APPEND COMBINED_SOURCES
    ${CIMGUI_ROOT}/cimplot/cimplot.cpp
    ${CIMGUI_ROOT}/cimplot/implot/implot.cpp
    ${CIMGUI_ROOT}/cimplot/implot/implot_demo.cpp
    ${CIMGUI_ROOT}/cimplot/implot/implot_items.cpp
  )
  list(APPEND COMBINED_INCLUDES
    ${CIMGUI_ROOT}/cimplot
    ${CIMGUI_ROOT}/cimplot/implot
  )
endif()

if(EXISTS "${CIMGUI_ROOT}/cimnodes/cimnodes.cpp" AND EXISTS "${CIMGUI_ROOT}/cimnodes/imnodes/imnodes.cpp")
  list(APPEND COMBINED_SOURCES
    ${CIMGUI_ROOT}/cimnodes/cimnodes.cpp
    ${CIMGUI_ROOT}/cimnodes/imnodes/imnodes.cpp
  )
  list(APPEND COMBINED_INCLUDES
    ${CIMGUI_ROOT}/cimnodes
    ${CIMGUI_ROOT}/cimnodes/imnodes
  )
  list(APPEND COMBINED_DEFS
    IMNODES_NAMESPACE=imnodes
  )
endif()

if(EXISTS "${CIMGUI_ROOT}/cimguizmo/cimguizmo.cpp" AND EXISTS "${CIMGUI_ROOT}/cimguizmo/ImGuizmo/ImGuizmo.cpp")
  list(APPEND COMBINED_SOURCES
    ${CIMGUI_ROOT}/cimguizmo/cimguizmo.cpp
    ${CIMGUI_ROOT}/cimguizmo/ImGuizmo/ImGuizmo.cpp
  )
  list(APPEND COMBINED_INCLUDES
    ${CIMGUI_ROOT}/cimguizmo
    ${CIMGUI_ROOT}/cimguizmo/ImGuizmo
  )
endif()

function(apply_android_link_opts target_name)
  if(ANDROID AND (CMAKE_ANDROID_ARCH_ABI STREQUAL "arm64-v8a" OR CMAKE_ANDROID_ARCH_ABI STREQUAL "x86_64"))
    target_link_options(${target_name} PRIVATE "-Wl,-z,max-page-size=16384")
  endif()
endfunction()

add_library(cimgui SHARED
  ${COMBINED_SOURCES}
)

apply_android_link_opts(cimgui)

target_include_directories(cimgui PUBLIC
  ${COMBINED_INCLUDES}
)

target_link_libraries(cimgui PRIVATE freetype)

add_definitions("-DCIMGUI_FREETYPE=1")
target_compile_definitions(cimgui PUBLIC
  ${COMBINED_DEFS}
)

set_target_properties(cimgui PROPERTIES
  OUTPUT_NAME "cimgui"
  PREFIX "lib"
)
CMAKE
}

build_cimgui_one_abi() {
  local ABI="${1:?ABI missing}"
  local BT="${2:?BuildType missing (Release/Debug)}"
  local BDIR="$CIMGUI_BUILD_ROOT/$ABI/$BT"

  local FT_PREFIX="$FT_INSTALL_ROOT/$ABI"
  local FT_CMAKE_DIR="$FT_PREFIX/lib/cmake/freetype"

  if [[ ! -d "$FT_CMAKE_DIR" ]]; then
    echo "ERROR: FreeType CMake config dir not found: $FT_CMAKE_DIR"
    exit 1
  fi

  rm -rf "$BDIR"
  mkdir -p "$BDIR"

  echo "=== Building libcimgui.so for $ABI ($BT) ==="
  cmake -S "$ANDROID_CMAKE_ROOT" -B "$BDIR" "${CMAKE_GEN[@]}" \
    -DCMAKE_TOOLCHAIN_FILE="$TOOLCHAIN" \
    -DANDROID_ABI="$ABI" \
    -DANDROID_PLATFORM="android-$ANDROID_API" \
    -DCMAKE_BUILD_TYPE="$BT" \
    -DCIMGUI_ROOT="$CIMGUI_DIR" \
    -Dfreetype_DIR="$FT_CMAKE_DIR" \
    -DCMAKE_PREFIX_PATH="$FT_PREFIX"

  cmake --build "$BDIR" --config "$BT" -- -j "$JOBS"

  # Copy into flavor-specific folder so Debug/Release don't overwrite each other
  local OUT_ABI_DIR="$CIMGUI_OUT_LIBROOT/$ABI/$BT"
  mkdir -p "$OUT_ABI_DIR"

  local FOUND=""
  if [[ -f "$BDIR/libcimgui.so" ]]; then
    FOUND="$BDIR/libcimgui.so"
  else
    FOUND="$(find "$BDIR" -type f -name "libcimgui.so" | head -n 1 || true)"
  fi

  if [[ -z "$FOUND" ]]; then
    echo "ERROR: libcimgui.so not found for $ABI ($BT)"
    exit 1
  fi

  cp -f "$FOUND" "$OUT_ABI_DIR/libcimgui.so"
  echo "Artifact: $OUT_ABI_DIR/libcimgui.so"
}

main() {
  echo "NDK: $ANDROID_NDK"
  echo "API: android-$ANDROID_API"
  echo "ABIS: $ABIS"
  echo "FreeType: $FREETYPE_VERSION"

  download_freetype

  for BT in $BUILD_FLAVORS; do
   for ABI in $ABIS; do
     build_freetype_one_abi "$ABI" "$BT"
   done
  done

  generate_android_cmakelists

  for BT in $BUILD_FLAVORS; do
   for ABI in $ABIS; do
     build_cimgui_one_abi "$ABI" "$BT"
   done
  done

  echo "Done."
  echo "Final output:"
  for BT in $BUILD_FLAVORS; do
     for ABI in $ABIS; do
          echo "  $CIMGUI_OUT_LIBROOT/$ABI/$BT/libcimgui.so"
     done 
  done
}

main "$@"
