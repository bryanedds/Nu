#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../.." && pwd)"

assimp_version="${ASSIMP_VERSION:-5.2.4}"
assimp_tag="${ASSIMP_TAG:-v$assimp_version}"
bulletsharp_rev="${BULLETSHARP_REV:-22261cf}"
cimgui_nativebuild_rev="${CIMGUI_NATIVEBUILD_REV:-3c24d703aa5ed86ba8b898ade871f90b26d12b1e}"
joltc_rev="${JOLTC_REV:-52d8c98}"
ios_deployment_target="${IOS_DEPLOYMENT_TARGET:-13.0}"
build_root="${BUILD_ROOT:-/private/tmp/nu-ios-native-build}"

if [ -z "${VULKAN_SDK:-}" ] && [ -f "$HOME/.bash_profile" ]; then
  source "$HOME/.bash_profile"
fi
vulkan_sdk="${VULKAN_SDK:-}"
if [ -z "$vulkan_sdk" ] || [ ! -f "$vulkan_sdk/macOS/include/vma/vk_mem_alloc.h" ]; then
  echo "VULKAN_SDK must point to a Vulkan SDK containing macOS/include/vma/vk_mem_alloc.h." >&2
  exit 1
fi

sdk_path="$(xcrun --sdk iphoneos --show-sdk-path)"
clang_path="$(xcrun --sdk iphoneos -find clang)"
clangxx_path="$(xcrun --sdk iphoneos -find clang++)"

assimp_src="$build_root/assimp-$assimp_version"
assimp_build="$build_root/assimp-ios-arm64"
bullet_src="$build_root/bullet3-src"
bulletsharp_src="$build_root/BulletSharpPInvoke"
bulletc_build="$build_root/bulletc-ios-arm64"
cimgui_src="$build_root/ImGui.NET-nativebuild"
cimgui_cmake="$build_root/cimgui-ios-cmake"
cimgui_build="$build_root/cimgui-ios-arm64"
vma_build="$build_root/vma-ios-arm64"
joltc_src="$build_root/joltc"
joltc_build="$build_root/joltc-ios-arm64"

assimp_dest="$repo_root/Nu/Nu.Dependencies/AssimpNet/iOS/iphoneos-arm64"
bulletc_dest="$repo_root/Nu/Nu.Dependencies/BulletSharpPInvoke/iOS/iphoneos-arm64"
cimgui_dest="$repo_root/Nu/Nu.Dependencies/ImGui/iOS/iphoneos-arm64"
vma_dest="$repo_root/Nu/Nu.Dependencies/Vortice.VulkanMemoryAllocator/iOS/iphoneos-arm64"
joltc_dest="$repo_root/Nu/Nu.Dependencies/JoltPhysics/iOS/iphoneos-arm64"

mkdir -p "$build_root" "$assimp_dest" "$bulletc_dest" "$cimgui_dest" "$vma_dest" "$joltc_dest"
rm -f \
  "$assimp_dest/libassimp.a" \
  "$bulletc_dest/libbulletc.a" \
  "$joltc_dest/libJolt.a" \
  "$joltc_dest/libjoltc.a"

if [ ! -d "$assimp_src/.git" ]; then
  git clone --depth 1 --branch "$assimp_tag" https://github.com/assimp/assimp.git "$assimp_src"
fi

if [ ! -d "$bulletsharp_src/.git" ]; then
  git clone https://github.com/AndresTraks/BulletSharpPInvoke.git "$bulletsharp_src"
fi
git -C "$bulletsharp_src" checkout "$bulletsharp_rev"

if [ ! -d "$cimgui_src/.git" ]; then
  git clone --recursive https://github.com/JoeTwizzle/ImGui.NET-nativebuild.git "$cimgui_src"
fi
git -C "$cimgui_src" checkout "$cimgui_nativebuild_rev"
git -C "$cimgui_src" submodule update --init --recursive

if [ ! -d "$joltc_src/.git" ]; then
  git clone https://github.com/amerkoleci/joltc.git "$joltc_src"
fi
git -C "$joltc_src" checkout "$joltc_rev"
perl -0pi -e 's/if \(IOS OR EMSCRIPTEN\)/if (EMSCRIPTEN)/g; s/Always Disable shared library on \(IOS, WEB\)/Always Disable shared library on WEB/g' "$joltc_src/CMakeLists.txt"

if [ ! -d "$bullet_src/bullet3" ]; then
  rm -rf "$bullet_src"
  mkdir -p "$bullet_src"
  unzip -q "$script_dir/bullet3.25.stripped.zip" -d "$bullet_src"
fi

rm -rf "$assimp_build"
cmake -S "$assimp_src" -B "$assimp_build" -G Ninja \
  -DCMAKE_SYSTEM_NAME=iOS \
  -DCMAKE_C_COMPILER="$clang_path" \
  -DCMAKE_CXX_COMPILER="$clangxx_path" \
  -DCMAKE_OSX_SYSROOT="$sdk_path" \
  -DCMAKE_OSX_ARCHITECTURES=arm64 \
  -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
  -DBUILD_SHARED_LIBS=OFF \
  -DASSIMP_BUILD_TESTS=OFF \
  -DASSIMP_BUILD_ASSIMP_TOOLS=OFF \
  -DASSIMP_NO_EXPORT=ON \
  -DASSIMP_BUILD_ZLIB=OFF \
  -DASSIMP_INSTALL=OFF \
  -DASSIMP_WARNINGS_AS_ERRORS=OFF
cmake --build "$assimp_build" --config Release --target assimp --parallel
"$clangxx_path" \
  -arch arm64 \
  -miphoneos-version-min="$ios_deployment_target" \
  -isysroot "$sdk_path" \
  -dynamiclib \
  -install_name @rpath/libassimp.dylib \
  -Wl,-force_load,"$assimp_build/lib/libassimp.a" \
  -stdlib=libc++ \
  -lz \
  -o "$assimp_dest/libassimp.dylib"

perl -0pi -e 's/ADD_LIBRARY\(\$\{BULLETC_LIB\} SHARED/ADD_LIBRARY\(\$\{BULLETC_LIB\} STATIC/' "$bulletsharp_src/libbulletc/CMakeLists.txt"

rm -rf "$bulletc_build"
cmake -S "$bulletsharp_src/libbulletc" -B "$bulletc_build" -G Ninja \
  -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
  -DCMAKE_SYSTEM_NAME=iOS \
  -DCMAKE_C_COMPILER="$clang_path" \
  -DCMAKE_CXX_COMPILER="$clangxx_path" \
  -DCMAKE_OSX_SYSROOT="$sdk_path" \
  -DCMAKE_OSX_ARCHITECTURES=arm64 \
  -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
  -DBUILD_SHARED_LIBS=OFF \
  -DBULLET_INCLUDE_DIR="$bullet_src/bullet3/src" \
  -DBUILD_BULLET2_DEMOS=OFF \
  -DBUILD_BULLET3=OFF \
  -DBUILD_CLSOCKET=OFF \
  -DBUILD_CPU_DEMOS=OFF \
  -DBUILD_ENET=OFF \
  -DBUILD_EXTRAS=ON \
  -DBUILD_OPENGL3_DEMOS=OFF \
  -DBUILD_UNIT_TESTS=OFF \
  -DINSTALL_LIBS=OFF \
  -DUSE_GRAPHICAL_BENCHMARK=OFF
cmake --build "$bulletc_build" --config Release --target libbulletc --parallel

xcrun --sdk iphoneos libtool -static -o "$bulletc_build/libbulletc_combined.a" \
  "$bulletc_build/liblibbulletc.a" \
  "$bulletc_build/bullet/src/BulletCollision/libBulletCollision.a" \
  "$bulletc_build/bullet/src/BulletDynamics/libBulletDynamics.a" \
  "$bulletc_build/bullet/src/BulletSoftBody/libBulletSoftBody.a" \
  "$bulletc_build/bullet/src/LinearMath/libLinearMath.a" \
  "$bulletc_build/bullet/Extras/HACD/libHACD.a" \
  "$bulletc_build/bullet/Extras/Serialize/BulletFileLoader/libBulletFileLoader.a" \
  "$bulletc_build/bullet/Extras/Serialize/BulletWorldImporter/libBulletWorldImporter.a" \
  "$bulletc_build/bullet/Extras/Serialize/BulletXmlWorldImporter/libBulletXmlWorldImporter.a"
"$clangxx_path" \
  -arch arm64 \
  -miphoneos-version-min="$ios_deployment_target" \
  -isysroot "$sdk_path" \
  -dynamiclib \
  -install_name @executable_path/libbulletc.dylib \
  -Wl,-force_load,"$bulletc_build/libbulletc_combined.a" \
  -stdlib=libc++ \
  -o "$bulletc_dest/libbulletc.dylib"

rm -rf "$cimgui_cmake" "$cimgui_build"
mkdir -p "$cimgui_cmake"
cat > "$cimgui_cmake/CMakeLists.txt" <<'CMAKE'
cmake_minimum_required(VERSION 3.20)
project(cimgui_ios C CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(NOT DEFINED CIMGUI_ROOT)
  message(FATAL_ERROR "CIMGUI_ROOT is not set.")
endif()

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
)

set(COMBINED_INCLUDES
  ${CIMGUI_ROOT}/cimgui
  ${CIMGUI_ROOT}/cimgui/imgui
)

set(COMBINED_DEFS
  IMGUI_DEFINE_MATH_OPERATORS=1
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

add_library(cimgui SHARED
  ${COMBINED_SOURCES}
)

target_include_directories(cimgui PUBLIC
  ${COMBINED_INCLUDES}
)

target_compile_definitions(cimgui PUBLIC
  ${COMBINED_DEFS}
)

set_target_properties(cimgui PROPERTIES
  OUTPUT_NAME "cimgui"
  PREFIX "lib"
  INSTALL_NAME_DIR "@rpath"
  MACOSX_RPATH ON
  XCODE_ATTRIBUTE_CODE_SIGNING_ALLOWED "NO"
)
CMAKE

cmake -S "$cimgui_cmake" -B "$cimgui_build" -G Ninja \
  -DCIMGUI_ROOT="$cimgui_src" \
  -DCMAKE_SYSTEM_NAME=iOS \
  -DCMAKE_C_COMPILER="$clang_path" \
  -DCMAKE_CXX_COMPILER="$clangxx_path" \
  -DCMAKE_OSX_SYSROOT="$sdk_path" \
  -DCMAKE_OSX_ARCHITECTURES=arm64 \
  -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
  -DCMAKE_BUILD_TYPE=Release
cmake --build "$cimgui_build" --config Release --parallel
cp "$cimgui_build/libcimgui.dylib" "$cimgui_dest/libcimgui.dylib"

rm -rf "$vma_build"
mkdir -p "$vma_build"
cat > "$vma_build/vma.cpp" <<'CPP'
#define VK_NO_PROTOTYPES 1
#define VMA_STATIC_VULKAN_FUNCTIONS 0
#define VMA_DYNAMIC_VULKAN_FUNCTIONS 1
#define VMA_CALL_PRE __attribute__((visibility("default")))
#define VMA_IMPLEMENTATION
#include <vma/vk_mem_alloc.h>
CPP
"$clangxx_path" \
  -std=c++17 \
  -arch arm64 \
  -miphoneos-version-min="$ios_deployment_target" \
  -isysroot "$sdk_path" \
  -fPIC \
  -dynamiclib \
  -fvisibility=hidden \
  -Wno-nullability-completeness \
  -I"$vulkan_sdk/macOS/include" \
  -install_name @rpath/libvma.dylib \
  "$vma_build/vma.cpp" \
  -o "$vma_build/libvma.dylib"
cp "$vma_build/libvma.dylib" "$vma_dest/libvma.dylib"

rm -rf "$joltc_build"
cmake -S "$joltc_src" -B "$joltc_build" -G Ninja \
  -DCMAKE_SYSTEM_NAME=iOS \
  -DCMAKE_C_COMPILER="$clang_path" \
  -DCMAKE_CXX_COMPILER="$clangxx_path" \
  -DCMAKE_OSX_SYSROOT="$sdk_path" \
  -DCMAKE_OSX_ARCHITECTURES=arm64 \
  -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
  -DCMAKE_BUILD_TYPE=Release \
  -DJPH_BUILD_SHARED=ON \
  -DJPH_SAMPLES=OFF \
  -DJPH_TESTS=OFF \
  -DJPH_INSTALL=OFF \
  -DUSE_SSE4_1=OFF \
  -DUSE_SSE4_2=OFF \
  -DUSE_AVX=OFF \
  -DUSE_AVX2=OFF \
  -DUSE_AVX512=OFF \
  -DUSE_LZCNT=OFF \
  -DUSE_TZCNT=OFF \
  -DUSE_F16C=OFF \
  -DUSE_FMADD=OFF
cmake --build "$joltc_build" --config Release --parallel
cp "$joltc_build/lib/libjoltc.dylib" "$joltc_dest/libjoltc.dylib"

file "$assimp_dest/libassimp.dylib"
file "$bulletc_dest/libbulletc.dylib"
file "$cimgui_dest/libcimgui.dylib"
file "$vma_dest/libvma.dylib"
file "$joltc_dest/libjoltc.dylib"
