#!/usr/bin/env bash
# ============================================================================
# Build.Native.Dependencies.sh
#
# Builds all of Nu's native dependencies and copies the resulting binaries
# into Nu/Nu.Dependencies, mirroring the layout Nu.targets expects:
#
#   AssimpNet/                  win-x64/                (assimp.dll, zlib1.dll)
#                               linux-x64/              (libassimp.so)
#                               android/<abi>/          (libassimp.so)
#                               iOS/assimp.xcframework  (ios + simulator + macos)
#   BulletSharpPInvoke/         win-x64/                (libbulletc.dll)
#                               linux-x64/              (libbulletc.so)
#                               iOS/bulletc.xcframework (ios + simulator + macos)
#   ImGui/                      android/<abi>/          (libcimgui.so)
#                               iOS/cimgui.xcframework  (ios + simulator)
#   JoltPhysics/                iOS/joltc.xcframework   (ios + simulator)
#   Vortice.VulkanMemoryAllocator/ iOS/vma.xcframework  (ios + simulator)
#   ShaderC/                    android/<abi>/          (libshaderc_shared.so, libc++_shared.so)
#                               iOS/shaderc_shared.xcframework (ios + simulator)
#
# Works both standalone and from CI (see .github/workflows/build-native.yml).
#
# Environment variables:
#   NAME                   - "win-x64" | "linux-x64" | "ios" | "android"
#                            (defaults to the host platform)
#   RUNNER_OS              - Windows | Linux | macOS (auto-detected if unset)
#   BUILD_TYPE             - Release (default) | Debug
#   IOS_DEPLOYMENT_TARGET  - iOS minimum OS version (default 13.0)
#   VULKAN_SDK             - required on macOS when building Apple targets (vma header)
#   ANDROID_NDK            - Android NDK root (auto-detected if unset)
#   ANDROID_API            - Android minimum API level (default 24)
#   ANDROID_ABIS           - Android ABIs to build (default "arm64-v8a x86_64")
#   ASSIMP_VERSION / ASSIMP_TAG / BULLETSHARP_REV / CIMGUI_NATIVEBUILD_REV / JOLTC_REV
#   BUILD_ROOT             - scratch directory for source checkouts / builds
# ============================================================================
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../.." && pwd)"
deps_root="$repo_root/Nu/Nu.Dependencies"

# -------------------------- platform detection --------------------------
name="${NAME:-}"
runner_os="${RUNNER_OS:-}"
if [ -z "$runner_os" ]; then
  case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*) runner_os="Windows" ;;
    Linux) runner_os="Linux" ;;
    Darwin) runner_os="macOS" ;;
    *) echo "ERROR: cannot detect OS, please set RUNNER_OS" >&2; exit 1 ;;
  esac
fi
if [ -z "$name" ]; then
  case "$runner_os" in
    Windows) name="win-x64" ;;
    Linux)   name="linux-x64" ;;
    macOS)   name="ios" ;;
  esac
fi

# -------------------------- config (env-overridable) --------------------------
assimp_version="${ASSIMP_VERSION:-5.2.4}"
assimp_tag="${ASSIMP_TAG:-v$assimp_version}"
bulletsharp_rev="${BULLETSHARP_REV:-22261cf}"
cimgui_nativebuild_rev="${CIMGUI_NATIVEBUILD_REV:-3c24d703aa5ed86ba8b898ade871f90b26d12b1e}"
joltc_rev="${JOLTC_REV:-52d8c98}"
ios_deployment_target="${IOS_DEPLOYMENT_TARGET:-13.0}"
android_api="${ANDROID_API:-24}"
android_abis="${ANDROID_ABIS:-arm64-v8a x86_64}"
if [ -n "${BUILD_ROOT:-}" ]; then
  build_root="$BUILD_ROOT"
else
  case "$runner_os" in
    Windows) build_root="${TEMP:-${TMP:-/tmp}}/nu-native-build" ;;
    *)       build_root="${TMPDIR:-/tmp}/nu-native-build" ;;
  esac
fi

# Vendored source archives (checked into Nu.External)
bullet3_zip="$repo_root/Nu/Nu.External/bullet3.25.stripped.zip"

# -------------------------- helpers --------------------------
need_cmd() {
  command -v "$1" >/dev/null 2>&1 || { echo "ERROR: missing '$1'" >&2; exit 1; }
}
log() { printf '\n==================== %s ====================\n' "$*"; }

# locate <build_dir> <filename-pattern> -> prints first matching file (or nothing)
locate() {
  find "$1" -name "$2" -type f 2>/dev/null | head -n 1
}

# source checkout locations
assimp_src="$build_root/assimp-$assimp_version"
bullet_src="$build_root/bullet3-src"
bulletsharp_src="$build_root/BulletSharpPInvoke"
cimgui_src="$build_root/ImGui.NET-nativebuild"
joltc_src="$build_root/joltc"
shaderc_src="$build_root/shaderc"

# prepare_sources <lib>...  where lib is one of: assimp bullet cimgui jolt shaderc
prepare_sources() {
  need_cmd git
  for lib in "$@"; do
    case "$lib" in
      assimp)
        if [ ! -d "$assimp_src/.git" ]; then
          git clone --depth 1 --branch "$assimp_tag" https://github.com/assimp/assimp.git "$assimp_src"
        fi
        ;;
      bullet)
        if [ ! -d "$bulletsharp_src/.git" ]; then
          git clone https://github.com/AndresTraks/BulletSharpPInvoke.git "$bulletsharp_src"
        fi
        git -C "$bulletsharp_src" checkout "$bulletsharp_rev"
        if [ ! -d "$bullet_src/bullet3" ]; then
          rm -rf "$bullet_src"
          mkdir -p "$bullet_src"
          need_cmd unzip
          unzip -q "$bullet3_zip" -d "$bullet_src"
        fi
        ;;
      cimgui)
        if [ ! -d "$cimgui_src/.git" ]; then
          git clone --recursive https://github.com/JoeTwizzle/ImGui.NET-nativebuild.git "$cimgui_src"
        fi
        git -C "$cimgui_src" checkout "$cimgui_nativebuild_rev"
        git -C "$cimgui_src" submodule update --init --recursive
        ;;
      jolt)
        if [ ! -d "$joltc_src/.git" ]; then
          git clone https://github.com/amerkoleci/joltc.git "$joltc_src"
        fi
        git -C "$joltc_src" checkout "$joltc_rev"
        # allow building a shared library on Apple platforms
        perl -0pi -e 's/if \(IOS OR EMSCRIPTEN\)/if (EMSCRIPTEN)/g; s/Always Disable shared library on \(IOS, WEB\)/Always Disable shared library on WEB/g' "$joltc_src/CMakeLists.txt"
        ;;
      shaderc)
        if [ ! -d "$shaderc_src/.git" ]; then
          git clone --depth 1 https://github.com/google/shaderc.git "$shaderc_src"
        fi
        need_cmd python3
        (cd "$shaderc_src" && python3 utils/git-sync-deps)
        ;;
    esac
  done
}

# -------------------------- desktop shared-library builders --------------------------
# These build assimp / bulletc as plain shared libraries for the host (or a
# cross-compiled desktop target such as macOS arm64). Extra args are passed
# verbatim to CMake (generator, architecture, deployment target, ...).
# Outputs land inside <build_dir>; callers copy them to Nu.Dependencies.

build_assimp_desktop() {
  local build_dir="$1"; shift
  rm -rf "$build_dir"
  cmake -S "$assimp_src" -B "$build_dir" \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_SHARED_LIBS=ON \
    -DASSIMP_BUILD_TESTS=OFF \
    -DASSIMP_BUILD_ASSIMP_TOOLS=OFF \
    -DASSIMP_NO_EXPORT=ON \
    -DASSIMP_BUILD_ZLIB=ON \
    -DASSIMP_INSTALL=OFF \
    -DASSIMP_WARNINGS_AS_ERRORS=OFF \
    "$@"
  cmake --build "$build_dir" --config Release --target assimp --parallel
}

build_bulletc_desktop() {
  local build_dir="$1"; shift
  rm -rf "$build_dir"
  cmake -S "$bulletsharp_src/libbulletc" -B "$build_dir" \
    -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
    -DCMAKE_BUILD_TYPE=Release \
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
    -DUSE_GRAPHICAL_BENCHMARK=OFF \
    "$@"
  cmake --build "$build_dir" --config Release --parallel
}

# copy the assimp shared library out of a desktop build dir into a destination
# file. Finds bin/ (Windows multi-config) and plain build-dir layouts.
copy_assimp_lib() {
  local build_dir="$1" dest_file="$2"
  local f
  f="$(locate "$build_dir" 'assimp.dll')"
  [ -n "$f" ] || f="$(locate "$build_dir" 'libassimp.so')"
  [ -n "$f" ] || f="$(locate "$build_dir" 'libassimp.dylib')"
  if [ -z "$f" ]; then echo "ERROR: assimp shared library not found in $build_dir" >&2; exit 1; fi
  cp "$f" "$dest_file"
  log "Copied $f -> $dest_file"
}

# copy the bulletc shared library out of a desktop build dir into a destination
# file. On Windows (Visual Studio generator) the CMake target is "libbulletc"
# and produces libbulletc.dll directly; with the Ninja generator the target is
# "libbulletc" which produces liblibbulletc.{so,dylib} (CMake adds "lib").
copy_bulletc_lib() {
  local build_dir="$1" dest_file="$2"
  local f
  f="$(locate "$build_dir" 'libbulletc.dll')"
  [ -n "$f" ] || f="$(locate "$build_dir" 'libbulletc.so')"
  [ -n "$f" ] || f="$(locate "$build_dir" 'libbulletc.dylib')"
  [ -n "$f" ] || f="$(locate "$build_dir" 'liblibbulletc.so')"
  [ -n "$f" ] || f="$(locate "$build_dir" 'liblibbulletc.dylib')"
  if [ -z "$f" ]; then echo "ERROR: bulletc shared library not found in $build_dir" >&2; exit 1; fi
  cp "$f" "$dest_file"
  log "Copied $f -> $dest_file"
}

# -------------------------- Windows x64 --------------------------
build_windows() {
  log "Building Windows x64 native dependencies"
  need_cmd cmake
  prepare_sources assimp bullet

  local assimp_build="$build_root/assimp-win-x64"
  local bullet_build="$build_root/bulletc-win-x64"
  local assimp_dest="$deps_root/AssimpNet/win-x64"
  local bullet_dest="$deps_root/BulletSharpPInvoke/win-x64"
  mkdir -p "$assimp_dest" "$bullet_dest"

  build_assimp_desktop "$assimp_build" \
    -A x64 \
    -DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded
  copy_assimp_lib "$assimp_build" "$assimp_dest/assimp.dll"

  build_bulletc_desktop "$bullet_build" \
    -A x64 \
    -DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded
  copy_bulletc_lib "$bullet_build" "$bullet_dest/libbulletc.dll"

  # zlib1.dll is retained for the legacy checked-in assimp.dll; freshly built
  # assimp bundles zlib statically (ASSIMP_BUILD_ZLIB=ON) and no longer needs it.
  if [ -f "$assimp_dest/zlib1.dll" ]; then
    log "Keeping legacy zlib1.dll at $assimp_dest/zlib1.dll"
  fi
}

# -------------------------- Linux x64 --------------------------
build_linux() {
  log "Building Linux x64 native dependencies"
  need_cmd cmake
  prepare_sources assimp bullet

  local assimp_build="$build_root/assimp-linux-x64"
  local bullet_build="$build_root/bulletc-linux-x64"
  local assimp_dest="$deps_root/AssimpNet/linux-x64"
  local bullet_dest="$deps_root/BulletSharpPInvoke/linux-x64"
  mkdir -p "$assimp_dest" "$bullet_dest"

  build_assimp_desktop "$assimp_build" -G Ninja
  copy_assimp_lib "$assimp_build" "$assimp_dest/libassimp.so"

  build_bulletc_desktop "$bullet_build" -G Ninja
  copy_bulletc_lib "$bullet_build" "$bullet_dest/libbulletc.so"
}

# -------------------------- Apple (iOS device + simulator, macOS slices) --------------------------
build_apple() {
  if [ "$runner_os" != "macOS" ]; then echo "ERROR: Apple targets require macOS" >&2; exit 1; fi
  log "Building Apple native dependencies (iOS device + simulator + macOS slices)"
  need_cmd xcrun
  need_cmd cmake
  need_cmd perl
  prepare_sources assimp bullet cimgui jolt shaderc

  # Vulkan SDK (provides the vk_mem_alloc.h header used by the vma library)
  if [ -z "${VULKAN_SDK:-}" ] && [ -f "$HOME/.bash_profile" ]; then
    source "$HOME/.bash_profile"
  fi
  vulkan_sdk="${VULKAN_SDK:-}"
  if [ -z "$vulkan_sdk" ] || [ ! -f "$vulkan_sdk/macOS/include/vma/vk_mem_alloc.h" ]; then
    echo "VULKAN_SDK must point to a Vulkan SDK containing macOS/include/vma/vk_mem_alloc.h. Select 'Vulkan Memory Allocator header' in the installer." >&2
    exit 1
  fi

  iphoneos_sdk_path="$(xcrun --sdk iphoneos --show-sdk-path)"
  iphonesimulator_sdk_path="$(xcrun --sdk iphonesimulator --show-sdk-path)"
  iphoneos_clang_path="$(xcrun --sdk iphoneos -find clang)"
  iphoneos_clangxx_path="$(xcrun --sdk iphoneos -find clang++)"
  iphonesimulator_clang_path="$(xcrun --sdk iphonesimulator -find clang)"
  iphonesimulator_clangxx_path="$(xcrun --sdk iphonesimulator -find clang++)"

  assimp_device_build="$build_root/assimp-ios-arm64"
  assimp_simulator_build="$build_root/assimp-iossimulator-arm64"
  bulletc_device_build="$build_root/bulletc-ios-arm64"
  bulletc_simulator_build="$build_root/bulletc-iossimulator-arm64"
  cimgui_cmake="$build_root/cimgui-ios-cmake"
  cimgui_device_build="$build_root/cimgui-ios-arm64"
  cimgui_simulator_build="$build_root/cimgui-iossimulator-arm64"
  vma_device_build="$build_root/vma-ios-arm64"
  vma_simulator_build="$build_root/vma-iossimulator-arm64"
  joltc_device_build="$build_root/joltc-ios-arm64"
  joltc_simulator_build="$build_root/joltc-iossimulator-arm64"
  shaderc_device_build="$build_root/shaderc-ios-arm64"
  shaderc_simulator_build="$build_root/shaderc-iossimulator-arm64"
  framework_build="$build_root/frameworks"

  assimp_dest="$deps_root/AssimpNet/iOS"
  bulletc_dest="$deps_root/BulletSharpPInvoke/iOS"
  cimgui_dest="$deps_root/ImGui/iOS"
  vma_dest="$deps_root/Vortice.VulkanMemoryAllocator/iOS"
  joltc_dest="$deps_root/JoltPhysics/iOS"
  shaderc_dest="$deps_root/ShaderC/iOS/shaderc_shared.xcframework"

  mkdir -p "$build_root" "$framework_build" \
    "$assimp_dest" "$bulletc_dest" "$cimgui_dest" "$vma_dest" "$joltc_dest" \
    "$(dirname "$shaderc_dest")"
  rm -rf \
    "$assimp_dest/iphoneos-arm64" \
    "$assimp_dest/iphonesimulator-arm64" \
    "$assimp_dest/assimp.xcframework" \
    "$bulletc_dest/iphoneos-arm64" \
    "$bulletc_dest/iphonesimulator-arm64" \
    "$bulletc_dest/bulletc.xcframework" \
    "$cimgui_dest/iphoneos-arm64" \
    "$cimgui_dest/iphonesimulator-arm64" \
    "$cimgui_dest/cimgui.xcframework" \
    "$vma_dest/iphoneos-arm64" \
    "$vma_dest/iphonesimulator-arm64" \
    "$vma_dest/vma.xcframework" \
    "$joltc_dest/iphoneos-arm64" \
    "$joltc_dest/iphonesimulator-arm64" \
    "$joltc_dest/joltc.xcframework" \
    "$shaderc_device_build" \
    "$shaderc_simulator_build" \
    "$shaderc_dest"

  # Build the macOS host binaries used as the macOS slices of the xcframeworks.
  # (No longer an external prerequisite: previously Nu/Nu/libassimp.dylib etc.)
  log "Building macOS arm64 slices (assimp, bulletc)"
  local assimp_macos_build="$build_root/assimp-macos-arm64"
  local bulletc_macos_build="$build_root/bulletc-macos-arm64"
  build_assimp_desktop "$assimp_macos_build" \
    -G Ninja \
    -DCMAKE_OSX_ARCHITECTURES=arm64 \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target"
  copy_assimp_lib "$assimp_macos_build" "$assimp_macos_build/libassimp.dylib"
  build_bulletc_desktop "$bulletc_macos_build" \
    -G Ninja \
    -DCMAKE_OSX_ARCHITECTURES=arm64 \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target"
  copy_bulletc_lib "$bulletc_macos_build" "$bulletc_macos_build/libbulletc.dylib"
  assimp_macos_binary="$assimp_macos_build/libassimp.dylib"
  bulletc_macos_binary="$bulletc_macos_build/libbulletc.dylib"

  create_dynamic_framework () {
    local name="$1"
    local binary="$2"
    local framework="$3"
    local platform="$4"

    rm -rf "$framework"
    mkdir -p "$framework"
    cp "$binary" "$framework/$name"
    chmod u+w "$framework/$name"
    /usr/bin/install_name_tool -id "@rpath/$name.framework/$name" "$framework/$name"
    cat > "$framework/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleDevelopmentRegion</key>
	<string>en</string>
	<key>CFBundleExecutable</key>
	<string>$name</string>
	<key>CFBundleIdentifier</key>
	<string>org.nu-game-engine.$name</string>
	<key>CFBundleInfoDictionaryVersion</key>
	<string>6.0</string>
	<key>CFBundleName</key>
	<string>$name</string>
	<key>CFBundlePackageType</key>
	<string>FMWK</string>
	<key>CFBundleShortVersionString</key>
	<string>1.0</string>
	<key>CFBundleVersion</key>
	<string>1</string>
	$(if [ "$platform" = "ios" ]; then printf '%s\n' '<key>MinimumOSVersion</key>' '<string>'"$ios_deployment_target"'</string>'; fi)
</dict>
</plist>
PLIST
  }

  create_dynamic_xcframework () {
    local name="$1"
    local device_binary="$2"
    local simulator_binary="$3"
    local macos_binary="$4"
    local output="$5"
    local device_framework="$framework_build/iphoneos/$name.framework"
    local simulator_framework="$framework_build/iphonesimulator/$name.framework"
    local macos_framework="$framework_build/macos/$name.framework"
    local xcodebuild_args=(-framework "$device_framework" -framework "$simulator_framework")

    rm -rf "$output"
    create_dynamic_framework "$name" "$device_binary" "$device_framework" "ios"
    create_dynamic_framework "$name" "$simulator_binary" "$simulator_framework" "ios"

    if [ -n "$macos_binary" ] && [ -f "$macos_binary" ]; then
      create_dynamic_framework "$name" "$macos_binary" "$macos_framework" "macos"
      xcodebuild_args+=( -framework "$macos_framework" )
    fi

    xcodebuild -create-xcframework \
      "${xcodebuild_args[@]}" \
      -output "$output"
  }

  # --- assimp ---
  log "Building assimp for iOS"
  rm -rf "$assimp_device_build"
  cmake -S "$assimp_src" -B "$assimp_device_build" -G Ninja \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_C_COMPILER="$iphoneos_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphoneos_clangxx_path" \
    -DCMAKE_OSX_SYSROOT="$iphoneos_sdk_path" \
    -DCMAKE_OSX_ARCHITECTURES=arm64 \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
    -DBUILD_SHARED_LIBS=OFF \
    -DASSIMP_BUILD_TESTS=OFF \
    -DASSIMP_BUILD_ASSIMP_TOOLS=OFF \
    -DASSIMP_NO_EXPORT=ON \
    -DASSIMP_BUILD_ZLIB=OFF \
    -DASSIMP_INSTALL=OFF \
    -DASSIMP_WARNINGS_AS_ERRORS=OFF
  cmake --build "$assimp_device_build" --config Release --target assimp --parallel
  "$iphoneos_clangxx_path" \
    -arch arm64 \
    -miphoneos-version-min="$ios_deployment_target" \
    -isysroot "$iphoneos_sdk_path" \
    -dynamiclib \
    -install_name @rpath/assimp.framework/assimp \
    -Wl,-force_load,"$assimp_device_build/lib/libassimp.a" \
    -stdlib=libc++ \
    -lz \
    -o "$assimp_device_build/assimp"

  rm -rf "$assimp_simulator_build"
  cmake -S "$assimp_src" -B "$assimp_simulator_build" -G Ninja \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_C_COMPILER="$iphonesimulator_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphonesimulator_clangxx_path" \
    -DCMAKE_OSX_SYSROOT="$iphonesimulator_sdk_path" \
    -DCMAKE_OSX_ARCHITECTURES=arm64 \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
    -DBUILD_SHARED_LIBS=OFF \
    -DASSIMP_BUILD_TESTS=OFF \
    -DASSIMP_BUILD_ASSIMP_TOOLS=OFF \
    -DASSIMP_NO_EXPORT=ON \
    -DASSIMP_BUILD_ZLIB=OFF \
    -DASSIMP_INSTALL=OFF \
    -DASSIMP_WARNINGS_AS_ERRORS=OFF
  cmake --build "$assimp_simulator_build" --config Release --target assimp --parallel
  "$iphonesimulator_clangxx_path" \
    -arch arm64 \
    -mios-simulator-version-min="$ios_deployment_target" \
    -isysroot "$iphonesimulator_sdk_path" \
    -dynamiclib \
    -install_name @rpath/assimp.framework/assimp \
    -Wl,-force_load,"$assimp_simulator_build/lib/libassimp.a" \
    -stdlib=libc++ \
    -lz \
    -o "$assimp_simulator_build/assimp"
  create_dynamic_xcframework "assimp" "$assimp_device_build/assimp" "$assimp_simulator_build/assimp" "$assimp_macos_binary" "$assimp_dest/assimp.xcframework"

  # --- bulletc ---
  log "Building bulletc for iOS"
  perl -0pi -e 's/ADD_LIBRARY\(\$\{BULLETC_LIB\} SHARED/ADD_LIBRARY\(\$\{BULLETC_LIB\} STATIC/' "$bulletsharp_src/libbulletc/CMakeLists.txt"

  rm -rf "$bulletc_device_build"
  cmake -S "$bulletsharp_src/libbulletc" -B "$bulletc_device_build" -G Ninja \
    -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_C_COMPILER="$iphoneos_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphoneos_clangxx_path" \
    -DCMAKE_OSX_SYSROOT="$iphoneos_sdk_path" \
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
  cmake --build "$bulletc_device_build" --config Release --target libbulletc --parallel

  xcrun --sdk iphoneos libtool -static -o "$bulletc_device_build/libbulletc_combined.a" \
    "$bulletc_device_build/liblibbulletc.a" \
    "$bulletc_device_build/bullet/src/BulletCollision/libBulletCollision.a" \
    "$bulletc_device_build/bullet/src/BulletDynamics/libBulletDynamics.a" \
    "$bulletc_device_build/bullet/src/BulletSoftBody/libBulletSoftBody.a" \
    "$bulletc_device_build/bullet/src/LinearMath/libLinearMath.a" \
    "$bulletc_device_build/bullet/Extras/HACD/libHACD.a" \
    "$bulletc_device_build/bullet/Extras/Serialize/BulletFileLoader/libBulletFileLoader.a" \
    "$bulletc_device_build/bullet/Extras/Serialize/BulletWorldImporter/libBulletWorldImporter.a" \
    "$bulletc_device_build/bullet/Extras/Serialize/BulletXmlWorldImporter/libBulletXmlWorldImporter.a"
  "$iphoneos_clangxx_path" \
    -arch arm64 \
    -miphoneos-version-min="$ios_deployment_target" \
    -isysroot "$iphoneos_sdk_path" \
    -dynamiclib \
    -install_name @rpath/bulletc.framework/bulletc \
    -Wl,-force_load,"$bulletc_device_build/libbulletc_combined.a" \
    -stdlib=libc++ \
    -o "$bulletc_device_build/bulletc"

  rm -rf "$bulletc_simulator_build"
  cmake -S "$bulletsharp_src/libbulletc" -B "$bulletc_simulator_build" -G Ninja \
    -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_C_COMPILER="$iphonesimulator_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphonesimulator_clangxx_path" \
    -DCMAKE_OSX_SYSROOT="$iphonesimulator_sdk_path" \
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
  cmake --build "$bulletc_simulator_build" --config Release --target libbulletc --parallel

  xcrun --sdk iphonesimulator libtool -static -o "$bulletc_simulator_build/libbulletc_combined.a" \
    "$bulletc_simulator_build/liblibbulletc.a" \
    "$bulletc_simulator_build/bullet/src/BulletCollision/libBulletCollision.a" \
    "$bulletc_simulator_build/bullet/src/BulletDynamics/libBulletDynamics.a" \
    "$bulletc_simulator_build/bullet/src/BulletSoftBody/libBulletSoftBody.a" \
    "$bulletc_simulator_build/bullet/src/LinearMath/libLinearMath.a" \
    "$bulletc_simulator_build/bullet/Extras/HACD/libHACD.a" \
    "$bulletc_simulator_build/bullet/Extras/Serialize/BulletFileLoader/libBulletFileLoader.a" \
    "$bulletc_simulator_build/bullet/Extras/Serialize/BulletWorldImporter/libBulletWorldImporter.a" \
    "$bulletc_simulator_build/bullet/Extras/Serialize/BulletXmlWorldImporter/libBulletXmlWorldImporter.a"
  "$iphonesimulator_clangxx_path" \
    -arch arm64 \
    -mios-simulator-version-min="$ios_deployment_target" \
    -isysroot "$iphonesimulator_sdk_path" \
    -dynamiclib \
    -install_name @rpath/bulletc.framework/bulletc \
    -Wl,-force_load,"$bulletc_simulator_build/libbulletc_combined.a" \
    -stdlib=libc++ \
    -o "$bulletc_simulator_build/bulletc"
  create_dynamic_xcframework "bulletc" "$bulletc_device_build/bulletc" "$bulletc_simulator_build/bulletc" "$bulletc_macos_binary" "$bulletc_dest/bulletc.xcframework"

  # --- cimgui ---
  log "Building cimgui for iOS"
  rm -rf "$cimgui_cmake" "$cimgui_device_build" "$cimgui_simulator_build"
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

  cmake -S "$cimgui_cmake" -B "$cimgui_device_build" -G Ninja \
    -DCIMGUI_ROOT="$cimgui_src" \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_C_COMPILER="$iphoneos_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphoneos_clangxx_path" \
    -DCMAKE_OSX_SYSROOT="$iphoneos_sdk_path" \
    -DCMAKE_OSX_ARCHITECTURES=arm64 \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
    -DCMAKE_BUILD_TYPE=Release
  cmake --build "$cimgui_device_build" --config Release --parallel

  cmake -S "$cimgui_cmake" -B "$cimgui_simulator_build" -G Ninja \
    -DCIMGUI_ROOT="$cimgui_src" \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_C_COMPILER="$iphonesimulator_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphonesimulator_clangxx_path" \
    -DCMAKE_OSX_SYSROOT="$iphonesimulator_sdk_path" \
    -DCMAKE_OSX_ARCHITECTURES=arm64 \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
    -DCMAKE_BUILD_TYPE=Release
  cmake --build "$cimgui_simulator_build" --config Release --parallel
  create_dynamic_xcframework "cimgui" "$cimgui_device_build/libcimgui.dylib" "$cimgui_simulator_build/libcimgui.dylib" "" "$cimgui_dest/cimgui.xcframework"

  # --- vma ---
  log "Building vma for iOS"
  rm -rf "$vma_device_build" "$vma_simulator_build"
  mkdir -p "$vma_device_build" "$vma_simulator_build"
  cat > "$vma_device_build/vma.cpp" <<'CPP'
#define VK_NO_PROTOTYPES 1
#define VMA_STATIC_VULKAN_FUNCTIONS 0
#define VMA_DYNAMIC_VULKAN_FUNCTIONS 1
#define VMA_CALL_PRE __attribute__((visibility("default")))
#define VMA_IMPLEMENTATION
#include <vma/vk_mem_alloc.h>
CPP
  cp "$vma_device_build/vma.cpp" "$vma_simulator_build/vma.cpp"
  "$iphoneos_clangxx_path" \
    -std=c++17 \
    -arch arm64 \
    -miphoneos-version-min="$ios_deployment_target" \
    -isysroot "$iphoneos_sdk_path" \
    -fPIC \
    -dynamiclib \
    -fvisibility=hidden \
    -Wno-nullability-completeness \
    -I"$vulkan_sdk/macOS/include" \
    -install_name @rpath/vma.framework/vma \
    "$vma_device_build/vma.cpp" \
    -o "$vma_device_build/vma"
  "$iphonesimulator_clangxx_path" \
    -std=c++17 \
    -arch arm64 \
    -mios-simulator-version-min="$ios_deployment_target" \
    -isysroot "$iphonesimulator_sdk_path" \
    -fPIC \
    -dynamiclib \
    -fvisibility=hidden \
    -Wno-nullability-completeness \
    -I"$vulkan_sdk/macOS/include" \
    -install_name @rpath/vma.framework/vma \
    "$vma_simulator_build/vma.cpp" \
    -o "$vma_simulator_build/vma"
  create_dynamic_xcframework "vma" "$vma_device_build/vma" "$vma_simulator_build/vma" "" "$vma_dest/vma.xcframework"

  # --- joltc ---
  log "Building joltc for iOS"
  rm -rf "$joltc_device_build"
  cmake -S "$joltc_src" -B "$joltc_device_build" -G Ninja \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_C_COMPILER="$iphoneos_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphoneos_clangxx_path" \
    -DCMAKE_OSX_SYSROOT="$iphoneos_sdk_path" \
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
  cmake --build "$joltc_device_build" --config Release --parallel

  rm -rf "$joltc_simulator_build"
  cmake -S "$joltc_src" -B "$joltc_simulator_build" -G Ninja \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_C_COMPILER="$iphonesimulator_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphonesimulator_clangxx_path" \
    -DCMAKE_OSX_SYSROOT="$iphonesimulator_sdk_path" \
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
  cmake --build "$joltc_simulator_build" --config Release --parallel
  create_dynamic_xcframework "joltc" "$joltc_device_build/lib/libjoltc.dylib" "$joltc_simulator_build/lib/libjoltc.dylib" "" "$joltc_dest/joltc.xcframework"

  file "$assimp_dest/assimp.xcframework/ios-arm64/assimp.framework/assimp"
  file "$assimp_dest/assimp.xcframework/ios-arm64-simulator/assimp.framework/assimp"
  file "$assimp_dest/assimp.xcframework/macos-arm64/assimp.framework/assimp"
  file "$bulletc_dest/bulletc.xcframework/ios-arm64/bulletc.framework/bulletc"
  file "$bulletc_dest/bulletc.xcframework/ios-arm64-simulator/bulletc.framework/bulletc"
  file "$bulletc_dest/bulletc.xcframework/macos-arm64/bulletc.framework/bulletc"
  file "$cimgui_dest/cimgui.xcframework/ios-arm64/cimgui.framework/cimgui"
  file "$cimgui_dest/cimgui.xcframework/ios-arm64-simulator/cimgui.framework/cimgui"
  file "$vma_dest/vma.xcframework/ios-arm64/vma.framework/vma"
  file "$vma_dest/vma.xcframework/ios-arm64-simulator/vma.framework/vma"
  file "$joltc_dest/joltc.xcframework/ios-arm64/joltc.framework/joltc"
  file "$joltc_dest/joltc.xcframework/ios-arm64-simulator/joltc.framework/joltc"

  # --- shaderc ---
  log "Building shaderc for iOS"
  rm -rf "$shaderc_device_build"
  cmake -S "$shaderc_src" -B "$shaderc_device_build" -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_COMPILER="$iphoneos_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphoneos_clangxx_path" \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_OSX_SYSROOT="$iphoneos_sdk_path" \
    -DCMAKE_OSX_ARCHITECTURES=arm64 \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
    -DSHADERC_SKIP_TESTS=ON \
    -DSHADERC_SKIP_EXAMPLES=ON \
    -DSHADERC_SKIP_COPYRIGHT_CHECK=ON
  cmake --build "$shaderc_device_build" --target shaderc_shared --parallel

  rm -rf "$shaderc_simulator_build"
  cmake -S "$shaderc_src" -B "$shaderc_simulator_build" -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_COMPILER="$iphonesimulator_clang_path" \
    -DCMAKE_CXX_COMPILER="$iphonesimulator_clangxx_path" \
    -DCMAKE_SYSTEM_NAME=iOS \
    -DCMAKE_OSX_SYSROOT="$iphonesimulator_sdk_path" \
    -DCMAKE_OSX_ARCHITECTURES=arm64 \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="$ios_deployment_target" \
    -DSHADERC_SKIP_TESTS=ON \
    -DSHADERC_SKIP_EXAMPLES=ON \
    -DSHADERC_SKIP_COPYRIGHT_CHECK=ON
  cmake --build "$shaderc_simulator_build" --target shaderc_shared --parallel

  create_dynamic_xcframework "shaderc_shared" "$shaderc_device_build/libshaderc/libshaderc_shared.dylib" "$shaderc_simulator_build/libshaderc/libshaderc_shared.dylib" "" "$shaderc_dest"

  file "$shaderc_dest/ios-arm64/shaderc_shared.framework/shaderc_shared"
  file "$shaderc_dest/ios-arm64-simulator/shaderc_shared.framework/shaderc_shared"
}

# -------------------------- Android --------------------------
build_android() {
  log "Building Android native dependencies ($android_abis)"
  need_cmd cmake
  prepare_sources assimp cimgui shaderc

  # Locate the Android NDK if ANDROID_NDK was not provided.
  : "${ANDROID_NDK:=}"
  if [ -z "$ANDROID_NDK" ]; then
    for p in \
      "${ANDROID_HOME:-}/ndk/"* \
      "/usr/local/lib/android/sdk/ndk/"* \
      "$HOME/Android/Sdk/ndk/"* \
      "/opt/android-ndk" \
      "/opt/android-ndk-r"* \
      ; do
      if [ -f "$p/build/cmake/android.toolchain.cmake" ]; then
        ANDROID_NDK="$p"
        break
      fi
    done
  fi
  local toolchain="$ANDROID_NDK/build/cmake/android.toolchain.cmake"
  if [ -z "$ANDROID_NDK" ] || [ ! -f "$toolchain" ]; then
    echo "ERROR: ANDROID_NDK not set or invalid. Expected: <NDK>/build/cmake/android.toolchain.cmake" >&2
    exit 1
  fi
  echo "Using Android NDK: $ANDROID_NDK"

  for abi in $android_abis; do
    local assimp_build="$build_root/assimp-android-$abi"
    local assimp_dest="$deps_root/AssimpNet/android/$abi"
    mkdir -p "$assimp_dest"

    log "Building assimp for Android $abi"
    rm -rf "$assimp_build"
    cmake -S "$assimp_src" -B "$assimp_build" -G Ninja \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_TOOLCHAIN_FILE="$toolchain" \
      -DANDROID_ABI="$abi" \
      -DANDROID_PLATFORM="android-$android_api" \
      -DBUILD_SHARED_LIBS=ON \
      -DASSIMP_BUILD_TESTS=OFF \
      -DASSIMP_BUILD_ASSIMP_TOOLS=OFF \
      -DASSIMP_NO_EXPORT=ON \
      -DASSIMP_BUILD_ZLIB=ON \
      -DASSIMP_INSTALL=OFF \
      -DASSIMP_WARNINGS_AS_ERRORS=OFF
    cmake --build "$assimp_build" --config Release --target assimp --parallel
    local f
    f="$(locate "$assimp_build" 'libassimp.so')"
    if [ -z "$f" ]; then echo "ERROR: libassimp.so not found for Android $abi" >&2; exit 1; fi
    cp "$f" "$assimp_dest/libassimp.so"
    log "Copied $f -> $assimp_dest/libassimp.so"

    local shaderc_build="$build_root/shaderc-android-$abi"
    local shaderc_dest="$deps_root/ShaderC/android/$abi"
    mkdir -p "$shaderc_dest"

    log "Building shaderc for Android $abi"
    rm -rf "$shaderc_build"
    cmake -S "$shaderc_src" -B "$shaderc_build" -G Ninja \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_TOOLCHAIN_FILE="$toolchain" \
      -DANDROID_ABI="$abi" \
      -DANDROID_PLATFORM="android-$android_api" \
      -DSHADERC_SKIP_TESTS=ON \
      -DSHADERC_SKIP_EXAMPLES=ON \
      -DSHADERC_SKIP_COPYRIGHT_CHECK=ON
    cmake --build "$shaderc_build" --target shaderc_shared --parallel
    f="$(locate "$shaderc_build" 'libshaderc_shared.so')"
    if [ -z "$f" ]; then echo "ERROR: libshaderc_shared.so not found for Android $abi" >&2; exit 1; fi
    cp "$f" "$shaderc_dest/libshaderc_shared.so"
    log "Copied $f -> $shaderc_dest/libshaderc_shared.so"

    # libc++_shared.so from the NDK (required at runtime by shaderc_shared)
    local ndk_abi
    case "$abi" in
      arm64-v8a) ndk_abi="aarch64-linux-android" ;;
      x86_64)    ndk_abi="x86_64-linux-android" ;;
      *)         ndk_abi="$abi" ;;
    esac
    local libcpp="$ANDROID_NDK/toolchains/llvm/prebuilt/linux-x86_64/sysroot/usr/lib/$ndk_abi/libc++_shared.so"
    if [ ! -f "$libcpp" ]; then
      libcpp="$(find "$ANDROID_NDK" -name 'libc++_shared.so' -path "*$ndk_abi*" | head -n 1)"
    fi
    if [ -n "$libcpp" ] && [ -f "$libcpp" ]; then
      cp "$libcpp" "$shaderc_dest/libc++_shared.so"
      log "Copied $libcpp -> $shaderc_dest/libc++_shared.so"
    else
      echo "WARNING: libc++_shared.so not found in NDK for $abi" >&2
    fi
  done

  # cimgui: delegate to the checked-in Android build script. It expects to be
  # run from the root of the ImGui.NET-nativebuild checkout, so copy it there.
  log "Building cimgui for Android (via build-android.sh)"
  cp "$deps_root/ImGui/android/build-android.sh" "$cimgui_src/build-android.sh"
  (
    cd "$cimgui_src"
    ANDROID_NDK="$ANDROID_NDK" \
    ANDROID_API="$android_api" \
    ABIS="$android_abis" \
    BUILD_FLAVORS=Release \
    bash build-android.sh
  )
  for abi in $android_abis; do
    local cimgui_out="$cimgui_src/_out_android/cimgui/$abi/Release/libcimgui.so"
    local cimgui_dest="$deps_root/ImGui/android/$abi"
    if [ ! -f "$cimgui_out" ]; then echo "ERROR: $cimgui_out not found" >&2; exit 1; fi
    mkdir -p "$cimgui_dest"
    cp "$cimgui_out" "$cimgui_dest/libcimgui.so"
    log "Copied $cimgui_out -> $cimgui_dest/libcimgui.so"
  done
}

# -------------------------- dispatch --------------------------
case "$name" in
  win-x64)        build_windows ;;
  linux-x64)      build_linux ;;
  ios|osx-arm64)  build_apple ;;
  android)        build_android ;;
  *)
    echo "ERROR: unknown NAME '$name'. Expected one of: win-x64, linux-x64, ios, android" >&2
    exit 1
    ;;
esac

log "Done: $name native dependencies written to Nu/Nu.Dependencies"
