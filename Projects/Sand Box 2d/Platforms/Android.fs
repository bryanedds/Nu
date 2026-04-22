namespace SandBox2dMobile

open Android.App
open Android.Content.PM
open System
open System.IO
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

// Use the MauiIcon ("mobile_icon_bg" is the file name): https://learn.microsoft.com/en-us/dotnet/maui/user-interface/images/app-icons?tabs=android#platform-specific-configuration
[<Application (Icon = "@mipmap/mobile_icon_bg", RoundIcon = "@mipmap/mobile_icon_bg_round")>] // Note: Application label is derived from project file
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
        // https://developer.android.com/guide/playcore/asset-delivery/test: Updates of asset packs are not supported. Before installing a new version of your build, manually uninstall the previous version.
        let assetPackManager = AssetPackManagerFactory.GetInstance this
        let mutable assetPackLocation = assetPackManager.GetPackLocation "gameassets"
        if isNull assetPackLocation then
            let tcs = System.Threading.Tasks.TaskCompletionSource<AssetPackLocation> ()
            let assetPackListener = new AssetPackStateUpdateListenerWrapper ()
            assetPackListener.StateUpdate.Add <| fun e ->
                if e.State.Name () = "gameassets" then
                    match e.State.Status () with
                    | Model.AssetPackStatus.Completed ->
                        let loc = assetPackManager.GetPackLocation "gameassets"
                        if isNull loc then
                            tcs.SetException (InvalidOperationException "Asset pack 'gameassets' completed but no pack location was available.")
                        else tcs.SetResult loc
                    | Model.AssetPackStatus.Failed ->
                        tcs.SetException (InvalidOperationException ($"Asset pack 'gameassets' failed with error code {e.State.ErrorCode ()}."))
                    | Model.AssetPackStatus.Canceled ->
                        tcs.SetException (OperationCanceledException "Asset pack 'gameassets' was canceled.")
                    | Model.AssetPackStatus.WaitingForWifi -> assetPackManager.ShowConfirmationDialog this |> ignore
                    | _ -> ()
            assetPackManager.RegisterListener assetPackListener.Listener
            assetPackManager.Fetch [|"gameassets"|] |> ignore
            // Wait synchronously in the SDL thread. This is not the main Android UI thread so the system won't kill the app.
            // TODO: show some loading UI?
            assetPackLocation <- tcs.Task.Result
            assetPackManager.UnregisterListener assetPackListener.Listener
        
        // set current directory for asset loading
        Directory.EnumerateDirectories (assetPackLocation.AssetsPath () + "/refinement-out", "*")
        |> Seq.exactlyOne
        |> Directory.SetCurrentDirectory

        // direct ConfigurationManager.AppSettings to load values from our App.config file
        if not (File.Exists "App.config") then
            raise (FileNotFoundException ($"Expected App.config at '{Directory.GetCurrentDirectory ()}' but it was not found. Something went wrong with asset pack loading."))
        AppDomain.CurrentDomain.SetData ("APP_CONFIG_FILE", System.IO.Path.GetFullPath "App.config") // "App.config" here will be interpreted as relative to AppDomain.CurrentDomain.BaseDirectory by .NET

        SandBox2d.Program.main () |> ignore<int>