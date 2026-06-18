module Nu.NativeLibraryLoading

open System
open System.IO
open System.Diagnostics.CodeAnalysis
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

let tryLoadNativeLibraryOpt libraryPath =
    let mutable handle = 0n
    if File.Exists libraryPath && NativeLibrary.TryLoad (libraryPath, &handle)
    then Some handle
    else None

let tryLoadNativeLibraryByNameOpt libraryName =
    let mutable handle = 0n
    if NativeLibrary.TryLoad (libraryName, &handle)
    then Some handle
        else None

module Apple =
    let frameworkPath frameworkName =
        Path.Combine (AppContext.BaseDirectory, "Frameworks", frameworkName + ".framework", frameworkName)

    let trySetDllImportResolver (assembly : Assembly) mappedLibraries =
        let resolver =
            DllImportResolver (fun libraryName _ _ ->
                mappedLibraries
                |> List.tryPick (fun (candidateName, frameworkName) ->
                    if String.Equals (libraryName, candidateName, StringComparison.Ordinal) then
                        if frameworkName = "__Internal" then
                            tryLoadNativeLibraryByNameOpt frameworkName
                        else tryLoadNativeLibraryOpt (frameworkPath frameworkName)
                    else None)
                |> Option.defaultValue 0n)
        try NativeLibrary.SetDllImportResolver (assembly, resolver)
        with :? InvalidOperationException -> ()

    let private loadAssimpFramework () =
        let assimpLibrary = global.Assimp.Unmanaged.AssimpLibrary.Instance
        if not assimpLibrary.IsLibraryLoaded then
            let libraryPath = frameworkPath "assimp"
            let libraryType = typeof<global.Assimp.Unmanaged.UnmanagedLibrary>
            let getField name =
                let field = libraryType.GetField (name, BindingFlags.Instance ||| BindingFlags.NonPublic)
                if isNull field then failwith ("Could not find AssimpNet field '" + name + "'.")
                field
            let impl = (getField "m_impl").GetValue assimpLibrary
            let loadLibraryMethod = impl.GetType().GetMethod ("LoadLibrary", BindingFlags.Instance ||| BindingFlags.Public)
            if isNull loadLibraryMethod then failwith "Could not find AssimpNet implementation LoadLibrary method."
            let loaded = loadLibraryMethod.Invoke (impl, [| box libraryPath |]) :?> bool
            if not loaded then failwith ("Could not load Assimp framework from '" + libraryPath + "'.")
            (getField "m_libraryPath").SetValue (assimpLibrary, libraryPath)
            (getField "m_checkNeedsLoading").SetValue (assimpLibrary, false)
            // AssimpNet's public LoadLibrary appends ".dylib" to extensionless paths,
            // but framework executables are intentionally extensionless.
            let onLibraryLoadedMethod = libraryType.GetMethod ("OnLibraryLoaded", BindingFlags.Instance ||| BindingFlags.NonPublic)
            if isNull onLibraryLoadedMethod then failwith "Could not find AssimpNet OnLibraryLoaded method."
            onLibraryLoadedMethod.Invoke (assimpLibrary, [||]) |> ignore

    [<DynamicDependency ("JoltDllImporterResolver", "JoltPhysicsSharp.JoltApi", "JoltPhysicsSharp")>] // iOS release mode linker will drop the event without this attribute
    let private configureJoltFramework () =
        let joltApiType = typeof<JoltPhysicsSharp.Jolt>.Assembly.GetType ("JoltPhysicsSharp.JoltApi", true)
        RuntimeHelpers.RunClassConstructor joltApiType.TypeHandle
        let joltResolverEvent = joltApiType.GetEvent ("JoltDllImporterResolver", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        if isNull joltResolverEvent then failwith "Could not find JoltPhysicsSharp JoltDllImporterResolver event."
        let resolver =
            DllImportResolver (fun libraryName _ _ ->
                assert (libraryName = "joltc")
                tryLoadNativeLibraryOpt (frameworkPath "joltc") |> Option.defaultValue 0n)
        joltResolverEvent.AddEventHandler (null, resolver)

    let private configureVmaFramework () =
        let vmaType = typeof<Vortice.Vulkan.Vma>
        RuntimeHelpers.RunClassConstructor vmaType.TypeHandle
        let vmaResolverEvent = vmaType.GetEvent ("VmaDllImporterResolver", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        if isNull vmaResolverEvent then failwith "Could not find Vortice Vulkan VmaDllImporterResolver event."
        let resolver =
            DllImportResolver (fun libraryName _ _ ->
                assert (libraryName = "vma")
                tryLoadNativeLibraryOpt (frameworkPath "vma") |> Option.defaultValue 0n)
        vmaResolverEvent.AddEventHandler (null, resolver)

    let configureFrameworkNativeLibraries () =
        trySetDllImportResolver typeof<World>.Assembly
            ["cimgui", "cimgui"]
        trySetDllImportResolver typeof<ImGuiNET.ImGui>.Assembly
            ["cimgui", "cimgui"]
        trySetDllImportResolver typeof<BulletSharp.BulletObject>.Assembly
            ["libbulletc", "bulletc"]
        loadAssimpFramework ()
        configureJoltFramework ()
        configureVmaFramework ()

module iOS =

    [<DynamicDependency ("ResolveLibrary", "Vortice.ShaderCompiler.Native", "Vortice.ShaderCompiler")>] // iOS release mode linker will drop the setter without this attribute
    let private configureShaderCompilerLibrary () =
        let shaderCompilerAssembly = typeof<Vortice.ShaderCompiler.ShaderMacro>.Assembly
        let nativeType = shaderCompilerAssembly.GetType ("Vortice.ShaderCompiler.Native", true)
        RuntimeHelpers.RunClassConstructor nativeType.TypeHandle
        let resolveLibraryProperty = nativeType.GetProperty ("ResolveLibrary", BindingFlags.Static ||| BindingFlags.Public)
        if isNull resolveLibraryProperty then failwith "Could not find Vortice.ShaderCompiler.Native.ResolveLibrary property."
        let resolver =
            DllImportResolver (fun libraryName _ _ ->
                assert (libraryName = "shaderc_shared")
                let hasShadercInitSymbol handle =
                    let mutable symbol = 0n
                    NativeLibrary.TryGetExport (handle, "shaderc_compiler_initialize", &symbol)
                [Apple.frameworkPath "shaderc_shared"; "shaderc_shared"; "@rpath/shaderc_shared.framework/shaderc_shared"]
                |> List.tryPick (fun candidate ->
                    tryLoadNativeLibraryOpt candidate
                    |> Option.filter hasShadercInitSymbol)
                |> Option.defaultValue 0n)
        resolveLibraryProperty.SetValue (null, resolver)

    let configureIosNativeLibraries () =
        Apple.trySetDllImportResolver typeof<SDL.SDL3>.Assembly
            ["SDL3", "SDL3"]
        Apple.trySetDllImportResolver typeof<SDL.SDL3_image>.Assembly
            ["SDL3_image", "SDL3_image"]
        Apple.trySetDllImportResolver typeof<SDL.SDL3_ttf>.Assembly
            ["SDL3_ttf", "SDL3_ttf"]
        Apple.trySetDllImportResolver typeof<SDL.SDL3_mixer>.Assembly
            ["SDL3_mixer", "SDL3_mixer"]
        configureShaderCompilerLibrary ()
        Apple.configureFrameworkNativeLibraries ()

        let moltenVkPath = Apple.frameworkPath "MoltenVK"
        let result = Vortice.Vulkan.Vulkan.vkInitialize moltenVkPath
        if result <> Vortice.Vulkan.VkResult.Success then
            failwith ("Could not initialize Vulkan from '" + moltenVkPath + "' due to: " + string result)
        SDL.SDL3.SDL_SetHint (SDL.SDL3.SDL_HINT_VULKAN_LIBRARY, moltenVkPath) |> ignore<SDL.SDLBool>

module Android =

    // We need this for Release mode with full AOT, but not necessary for Debug mode.
    let configureAndroidNativeLibraries () =
        let trySetAndroidDllImportResolver (assembly : Assembly) mappedLibraries =
            let resolver =
                DllImportResolver (fun libraryName _ _ ->
                    mappedLibraries
                    |> List.tryPick (fun (requestedName, candidateName) ->
                        if String.Equals (libraryName, requestedName, StringComparison.Ordinal)
                        then tryLoadNativeLibraryByNameOpt candidateName
                        else None)
                    |> Option.orElseWith (fun () -> tryLoadNativeLibraryByNameOpt libraryName)
                    |> Option.defaultValue 0n)
            try NativeLibrary.SetDllImportResolver (assembly, resolver)
            with :? InvalidOperationException -> ()

        // Normalize desktop-oriented sonames requested by some native bindings.
        let androidAliases =
            ["libc.so.6", "libc"
             "libc.so", "libc"
             "libdl.so", "libdl"
             "libvulkan.so.1", "libvulkan"
             "libvulkan.so", "libvulkan"
             "liblog", "liblog"
             "liblog.so", "liblog"]
        trySetAndroidDllImportResolver typeof<global.Assimp.Unmanaged.AssimpLibrary>.Assembly androidAliases
        trySetAndroidDllImportResolver typeof<Vortice.Vulkan.Vulkan>.Assembly androidAliases
        trySetAndroidDllImportResolver typeof<World>.Assembly androidAliases

