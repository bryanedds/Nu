module SandBox2d.Program
open System
open System.IO
open Nu

// this the entry point for your Nu application
let main () =

    // regardless of where the program is launched from in the command line, always resolve files relative to the application's base directory
    // for Android, we set the current directory to the asset pack location in MainActivity, so we skip this step here
    if not (OperatingSystem.IsAndroid ()) then Directory.SetCurrentDirectory AppContext.BaseDirectory

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

#if ANDROID
open Android.App
open Android.Content.PM
open Android.Widget
open System
open System.IO
open System.Threading.Tasks
open Xamarin.Google.Android.Play.Core.AssetPacks

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
    override this.GetLibraries () = [|"SDL3"; "SDL3_image"; "SDL3_ttf"; "SDL3_mixer"|] // SDL - Load these native libraries
    override this.Main () =
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
open SDL
open FSharp.NativeInterop
open System.Runtime.InteropServices

// SDL usage taken from https://github.com/ppy/SDL3-CS/blob/master/SDL3-CS.Tests.iOS/Main.cs
type SdlMain = delegate of argc : int * argv : byte nativeptr nativeptr -> int
let private sdlMain = SdlMain (fun _ _ -> main ())
   
let [<EntryPoint>] entryPoint _ =
    NativeLibrary.SetDllImportResolver (typeof<SDL3>.Assembly, fun _ assembly path -> NativeLibrary.Load ("Frameworks/SDL3.framework/SDL3", assembly, path))
    NativeLibrary.SetDllImportResolver (typeof<SDL3_image>.Assembly, fun _ assembly path -> NativeLibrary.Load ("Frameworks/SDL3_image.framework/SDL3_image", assembly, path))
    NativeLibrary.SetDllImportResolver (typeof<SDL3_ttf>.Assembly, fun _ assembly path -> NativeLibrary.Load ("Frameworks/SDL3_ttf.framework/SDL3_ttf", assembly, path))
    NativeLibrary.SetDllImportResolver (typeof<SDL3_mixer>.Assembly, fun _ assembly path -> NativeLibrary.Load ("Frameworks/SDL3_mixer.framework/SDL3_mixer", assembly, path))
    if ObjCRuntime.Runtime.Arch = ObjCRuntime.Arch.SIMULATOR then
        let result = Vortice.Vulkan.Vulkan.vkInitialize "Frameworks/MoltenVK.framework/MoltenVK"
        assert (result = Vortice.Vulkan.VkResult.Success)

    SDL3.SDL_RunApp (0, NativePtr.nullPtr, Marshal.GetFunctionPointerForDelegate<_> sdlMain, 0n)
#endif
#if !(ANDROID || IOS)
let [<EntryPoint>] entryPoint _ = main ()
#endif