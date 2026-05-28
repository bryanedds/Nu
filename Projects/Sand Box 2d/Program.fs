module SandBox2d.Program
open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Nu

// this the entry point for your Nu application
let main () =
    Constants.Render.SkipRendering3d <- true // skipping the 3D renderer for startup time minimization

    // this initializes Nu before other Nu code is run
    Nu.init ()

    // this specifies the window configuration used to display the game
    let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Sand Box (2D)" }

    // this specifies the configuration of the game engine's use of SDL
    let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }

    // this specifies the world config using the above SDL config
    let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

    // this runs the engine with the given config and plugin, starting the game
    World.run worldConfig (SandBox2dPlugin ())

let private frameworkPath frameworkName =
    Path.Combine (AppContext.BaseDirectory, "Frameworks", frameworkName + ".framework", frameworkName)

let private tryLoadNativeLibraryOpt libraryPath =
    let mutable handle = 0n
    if File.Exists libraryPath && NativeLibrary.TryLoad (libraryPath, &handle)
    then Some handle
    else None

let private tryLoadNativeLibraryByNameOpt libraryName =
    let mutable handle = 0n
    if NativeLibrary.TryLoad (libraryName, &handle)
    then Some handle
    else None

let private trySetDllImportResolver (assembly : Assembly) mappedLibraries =
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

let private configureFrameworkNativeLibraries () =
    trySetDllImportResolver typeof<World>.Assembly
        ["cimgui", "cimgui"]
    trySetDllImportResolver typeof<ImGuiNET.ImGui>.Assembly
        ["cimgui", "cimgui"]
    trySetDllImportResolver typeof<BulletSharp.BulletObject>.Assembly
        ["libbulletc", "bulletc"]
    loadAssimpFramework ()
    configureJoltFramework ()
    configureVmaFramework ()

#if ANDROID
open Android.App
open Android.Content.PM
open Android.Widget
open System
open System.IO
open System.Threading.Tasks
open Xamarin.Google.Android.Play.Core.AssetPacks

// We need this for Release mode with full AOT, but not necessary for Debug mode.
let private configureAndroidNativeLibraries () =
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

type PreDrawListener (dispose : PreDrawListener -> unit) =
    inherit Java.Lang.Object ()
    interface Android.Views.ViewTreeObserver.IOnPreDrawListener with
        member this.OnPreDraw () =
            if Globals.Render.IsReady then dispose this
            Globals.Render.IsReady

// Android Manifest through .NET attributes: https://learn.microsoft.com/en-us/dotnet/maui/android/manifest#attributes
// Refer to https://github.com/libsdl-org/SDL/blob/main/android-project/app/src/main/AndroidManifest.xml for all needed attributes for SDL

// Declare wanted features: https://developer.android.com/guide/topics/manifest/uses-feature-element#features-reference
[<UsesFeature (PackageManager.FeatureVulkanHardwareVersion, Required = true)>] // TODO: Version pending https://github.com/dotnet/android/pull/10890
[<UsesFeature (PackageManager.FeatureTouchscreen, Required = false)>] // SDL - Declare touch screen support
[<UsesFeature (PackageManager.FeatureBluetooth, Required = false)>] // SDL - Declare game controller support
[<UsesFeature (PackageManager.FeatureGamepad, Required = false)>] // SDL - Declare game controller support
[<UsesFeature (PackageManager.FeatureUsbHost, Required = false)>] // SDL - Declare game controller support
[<UsesFeature (PackageManager.FeaturePc, Required = false)>] // SDL - Declare external mouse input events support

// Declare wanted permissions: https://developer.android.com/reference/android/Manifest.permission
//[<UsesPermission (Android.Manifest.Permission.Internet)>] // for example

// Note: Label property is derived from project file
[<Application (Icon = "@mipmap/mobile_icon_bg", RoundIcon = "@mipmap/mobile_icon_bg_round", // Use the MauiIcon ("mobile_icon_bg" is the file name): https://learn.microsoft.com/en-us/dotnet/maui/user-interface/images/app-icons?tabs=android#platform-specific-configuration
               AppCategory = ApplicationCategories.Game)>] // For system summaries like for battery, network, or disk usage
do ()

// Entry point, SDL usage taken from https://github.com/ppy/SDL3-CS/blob/master/SDL3-CS.Tests.Android/MainActivity.cs
[<Activity (LaunchMode = LaunchMode.SingleInstance, // Only allow one instance of the game to be launched at once
            MainLauncher = true, // At least one activity must be marked as the main launcher to be able to start the app (when the user taps the app icon).
            Theme = "@style/Maui.SplashTheme", // SplashTheme: https://learn.microsoft.com/en-us/dotnet/maui/user-interface/images/splashscreen?tabs=android#platform-specific-configuration
            ConfigurationChanges = enum -1, // SDL - Do not recreate the activity on all configuration changes, since SDL handles them itself.
            ScreenOrientation = ScreenOrientation.UserLandscape)>] // Orientation before SDL initialization where it overrides based on SDL_HINT_ORIENTATIONS
[<IntentFilter ([|Android.Hardware.Usb.UsbManager.ActionUsbDeviceAttached|])>] // SDL - Let Android know that we can handle some USB devices and should receive this event
type MainActivity () =
    inherit Org.Libsdl.App.SDLActivity ()
    override this.OnCreate (savedInstanceState: Android.OS.Bundle) = 
        base.OnCreate savedInstanceState // Sets activity content
        // Don't draw SDL's black window by preserving the splash screen until the first frame is available: https://developer.android.com/develop/ui/views/launch/splash-screen?utm_source=copilot.com#suspend-drawing
        let content = this.FindViewById Android.Resource.Id.Content
        content.ViewTreeObserver.AddOnPreDrawListener (new PreDrawListener (fun listener -> content.ViewTreeObserver.RemoveOnPreDrawListener listener)) // can't simplify the lambda since the ViewTreeObserver must be alive by getting the property!
    override this.GetLibraries () = [|"SDL3"; "SDL3_image"; "SDL3_ttf"; "SDL3_mixer"|] // SDL - Load these native libraries
    override this.Main () =
        configureAndroidNativeLibraries ()

        // Get the file system path for fast-follow asset pack "gameassets". Customize this if you use a different asset pack. For on-demand asset packs, you would need to trigger the download and wait for completion before getting the path.
        // How to use asset pack manager: https://developer.android.com/guide/playcore/asset-delivery/integrate-java
        // NOTE: For debugging, updates of asset packs are not supported. Before installing a new version of your build, manually uninstall the previous version. See https://developer.android.com/guide/playcore/asset-delivery/test
        let assetPackManager = AssetPackManagerFactory.GetInstance this
        let mutable assetPackLocation = assetPackManager.GetPackLocation "gameassets"
        if isNull assetPackLocation then
            let tcs = TaskCompletionSource<AssetPackLocation> ()
            let mutable loadingDialogOpt = None

            // show loading ui
            this.RunOnUiThread (fun () ->
                // set up loading layout
                let layout = new LinearLayout (this, Orientation = Orientation.Vertical)
                let density = this.Resources.DisplayMetrics.Density
                let padding = int (16.0f * density)
                layout.SetPadding (padding, padding, padding, padding)

                // add loading text and progress bar to layout
                let loadingText = new TextView (this, Text = "Preparing assets download...")
                layout.AddView loadingText
                let loadingProgressBar = new ProgressBar (this, null, Android.Resource.Attribute.ProgressBarStyleHorizontal)
                layout.AddView (loadingProgressBar, new LinearLayout.LayoutParams (Android.Views.ViewGroup.LayoutParams.MatchParent, Android.Views.ViewGroup.LayoutParams.WrapContent, TopMargin = int (8.0f * density)))

                // display layout as dialog
                let builder = new AlertDialog.Builder (this)
                builder.SetTitle "Installing assets" |> ignore
                builder.SetCancelable false |> ignore
                builder.SetView layout |> ignore
                let loadingDialog = builder.Create ()
                loadingDialogOpt <- Some (loadingDialog, loadingText, loadingProgressBar)
                loadingDialog.Show ())

            // update loading ui
            let assetPackListener = new AssetPackStateUpdateListenerWrapper ()
            assetPackListener.StateUpdate.Add <| fun e ->
                if e.State.Name () = "gameassets" then
                    let downloadProgress =
                        if e.State.TotalBytesToDownload () > 0L then int (100L * e.State.BytesDownloaded () / e.State.TotalBytesToDownload ()) else 100
                    let updateLoadingUi status progress =
                        this.RunOnUiThread (fun () ->
                            match loadingDialogOpt with
                            | Some (_, loadingText, loadingProgressBar) ->
                                loadingText.Text <- $"{status} {progress}%%"
                                loadingProgressBar.Progress <- progress
                            | None -> ())
                    match e.State.Status () with
                    | Model.AssetPackStatus.Pending -> updateLoadingUi "Pending assets download..." 0
                    | Model.AssetPackStatus.Downloading -> updateLoadingUi $"Downloading assets..." downloadProgress
                    | Model.AssetPackStatus.WaitingForWifi ->
                        updateLoadingUi $"Waiting for Wi-Fi connection..." downloadProgress
                        assetPackManager.ShowConfirmationDialog this |> ignore
                    | Model.AssetPackStatus.RequiresUserConfirmation ->
                        updateLoadingUi $"Waiting for user confirmation..." downloadProgress
                        assetPackManager.ShowConfirmationDialog this |> ignore
                    | Model.AssetPackStatus.Transferring -> updateLoadingUi $"Download complete. Installing assets..." (e.State.TransferProgressPercentage ())
                    | Model.AssetPackStatus.Completed -> tcs.TrySetResult (assetPackManager.GetPackLocation "gameassets") |> ignore
                    | Model.AssetPackStatus.Failed -> updateLoadingUi $"Installation failed with error code {e.State.ErrorCode ()}." downloadProgress
                    | Model.AssetPackStatus.Canceled -> updateLoadingUi $"Installation canceled." downloadProgress
                    | status -> updateLoadingUi $"Unknown installation status {status}..." downloadProgress

            // fetch assets then wait synchronously in the main SDL thread. this is not the main Android UI thread so the system won't kill the app.
            assetPackManager.RegisterListener assetPackListener.Listener
            assetPackManager.Fetch [|"gameassets"|] |> ignore
            assetPackLocation <- tcs.Task.Result
            assetPackManager.UnregisterListener assetPackListener.Listener

            // hide loading ui
            match loadingDialogOpt with
            | Some (loadingDialog, _, _) ->
                loadingDialogOpt <- None
                loadingDialog.Dismiss () // thread safe
            | None -> ()

        // set current directory for asset loading
        Directory.EnumerateDirectories (assetPackLocation.AssetsPath () + "/refinement-out", "*")
        |> Seq.exactlyOne
        |> Directory.SetCurrentDirectory

        // direct ConfigurationManager.AppSettings to load values from our App.config file
        if not (File.Exists "App.config") then
            raise (FileNotFoundException ($"Expected App.config at '{Directory.GetCurrentDirectory ()}' but it was not found. Something went wrong with asset pack loading."))
        AppDomain.CurrentDomain.SetData ("APP_CONFIG_FILE", System.IO.Path.GetFullPath "App.config") // "App.config" here will be interpreted as relative to AppDomain.CurrentDomain.BaseDirectory by .NET

        main () |> ignore<int>
#endif
#if IOS
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
            [frameworkPath "shaderc_shared"; "shaderc_shared"; "@rpath/shaderc_shared.framework/shaderc_shared"]
            |> List.tryPick (fun candidate ->
                tryLoadNativeLibraryOpt candidate
                |> Option.filter hasShadercInitSymbol)
            |> Option.defaultValue 0n)
    resolveLibraryProperty.SetValue (null, resolver)

let private configureIosNativeLibraries () =
    trySetDllImportResolver typeof<SDL.SDL3>.Assembly
        ["SDL3", "SDL3"]
    trySetDllImportResolver typeof<SDL.SDL3_image>.Assembly
        ["SDL3_image", "SDL3_image"]
    trySetDllImportResolver typeof<SDL.SDL3_ttf>.Assembly
        ["SDL3_ttf", "SDL3_ttf"]
    trySetDllImportResolver typeof<SDL.SDL3_mixer>.Assembly
        ["SDL3_mixer", "SDL3_mixer"]
    configureShaderCompilerLibrary ()
    configureFrameworkNativeLibraries ()

    let moltenVkPath = frameworkPath "MoltenVK"
    let result = Vortice.Vulkan.Vulkan.vkInitialize moltenVkPath
    if result <> Vortice.Vulkan.VkResult.Success then
        failwith ("Could not initialize Vulkan from '" + moltenVkPath + "' due to: " + string result)
    SDL.SDL3.SDL_SetHint (SDL.SDL3.SDL_HINT_VULKAN_LIBRARY, moltenVkPath) |> ignore<SDL.SDLBool>

open SDL
open FSharp.NativeInterop

// SDL usage taken from https://github.com/ppy/SDL3-CS/blob/master/SDL3-CS.Tests.iOS/Main.cs
type SdlMain = delegate of argc : int * argv : byte nativeptr nativeptr -> int
let private sdlMain =
    SdlMain (fun _ _ ->
        // this points the current working directory at the bundled game assets
        let baseDirectory = AppContext.BaseDirectory
        let assetDirectory = Path.Combine (baseDirectory, "refinement-out", "net10.0-ios")
        Directory.SetCurrentDirectory assetDirectory

        main ())
   
let [<EntryPoint>] entryPoint _ =
    Log.init None // disable Nu's default file log because the iOS app bundle is read-only.
    configureIosNativeLibraries ()
    if ObjCRuntime.Runtime.Arch = ObjCRuntime.Arch.SIMULATOR then
        // Avoid hitting MoltenVK iOS Simulator limitations like:
        // - only 31 buffers are supported in the simulator
        // - no rendering to array (layered) attachments
        Constants.Render.SkipRendering3d <- true

    SDL3.SDL_RunApp (0, NativePtr.nullPtr, Marshal.GetFunctionPointerForDelegate<_> sdlMain, 0n)
#endif
#if !(ANDROID || IOS)
let [<EntryPoint>] entryPoint _ =
    if OperatingSystem.IsMacOS () then
        configureFrameworkNativeLibraries ()
    // regardless of where the program is launched from in the command line, always resolve files relative to the application's base directory
    Directory.SetCurrentDirectory AppContext.BaseDirectory
    main ()
#endif
