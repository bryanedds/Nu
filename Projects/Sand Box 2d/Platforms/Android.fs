namespace SandBox2dMobile

open Android.App
open Android.OS
open Android.Content.PM
open System
open System.Threading
open System.Threading.Tasks

// Android Manifest through .NET attributes: https://learn.microsoft.com/en-us/dotnet/maui/android/manifest#attributes

// Use the MauiIcon ("mobile_icon_bg" is the file name): https://learn.microsoft.com/en-us/dotnet/maui/user-interface/images/app-icons?tabs=android#platform-specific-configuration
[<Application (Icon = "@mipmap/mobile_icon_bg", RoundIcon = "@mipmap/mobile_icon_bg_round")>]
// Declare wanted features: https://developer.android.com/guide/topics/manifest/uses-feature-element#features-reference
[<UsesFeature (PackageManager.FeatureVulkanHardwareVersion, Required = true)>] // TODO: Version pending https://github.com/dotnet/android/pull/10890
// Declare wanted permissions: https://developer.android.com/reference/android/Manifest.permission
//[<UsesPermission (Android.Manifest.Permission.Internet)>] // for example
do ()

open Xamarin.Google.Android.Play.Core.AssetPacks

// Entry point, SDL usage taken from https://github.com/ppy/SDL3-CS/blob/master/SDL3-CS.Tests.Android/MainActivity.cs
[<Activity (LaunchMode = LaunchMode.SingleInstance, MainLauncher = true, Theme = "@style/Maui.SplashTheme")>] // SplashTheme: https://learn.microsoft.com/en-us/dotnet/maui/user-interface/images/splashscreen?tabs=android#platform-specific-configuration
type MainActivity () =
    inherit Org.Libsdl.App.SDLActivity ()
    override this.GetLibraries () = [|"SDL3"; "SDL3_image"; "SDL3_ttf"; "SDL3_mixer"|]
    override this.Main () =

        // Get the file system path for fast-follow asset pack "gameassets". Customize this if you use a different asset pack. For on-demand asset packs, you would need to trigger the download and wait for completion before getting the path.
        let assetPackManager = AssetPackManagerFactory.GetInstance this
        let mutable assetPackLocation = assetPackManager.GetPackLocation "gameassets"
        if isNull assetPackLocation then
            // Wait synchronously in the SDL thread. This is not the main Android UI thread so the system won't kill the app.
            // TODO: show some loading UI?
            let states = assetPackManager.Fetch([|"gameassets"|]).Result :?> AssetPackStates
            let state = states.PackStates().["gameassets"]
            assetPackLocation <-
                match state.Status () with
                | Model.AssetPackStatus.Completed ->
                    let loc = assetPackManager.GetPackLocation "gameassets"
                    if isNull loc then
                        raise (InvalidOperationException "Asset pack 'gameassets' completed but no pack location was available.")
                    else loc
                | Model.AssetPackStatus.Failed ->
                    raise (InvalidOperationException ($"Asset pack 'gameassets' failed with error code {state.ErrorCode ()}."))
                | Model.AssetPackStatus.Canceled ->
                    raise (OperationCanceledException "Asset pack 'gameassets' was canceled.")
                | x -> raise (InvalidOperationException $"Asset pack 'gameassets' completed with unknown state {x}.")
        System.IO.Directory.SetCurrentDirectory (assetPackLocation.AssetsPath () + "/refinement-out")

        Nu.Constants.Engine.MobileBuild <- true
        SandBox2d.Program.main () |> ignore<int>