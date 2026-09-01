#!/usr/bin/env -S dotnet fsi

// Dynamically select a compatible Xamarin.Google.Android.Play.Asset.Delivery version
// that restores for Android without NU1608 warnings. No versions or ranges are hard-coded.
//
// Usage:
//  dotnet fsi SelectCompatibleAssetDeliveryVersion.fsx --project-path <path_to_.fsproj> --tfm <target_framework>

open System
open System.IO
open System.Net.Http
open System.Diagnostics
open System.Text.RegularExpressions

// ---- CLI ----

let args = fsi.CommandLineArgs |> Array.skip 1

let projectPath =
    let idx = args |> Array.findIndex (fun a -> a = "--project-path")
    if idx >= 0 && idx + 1 < args.Length then args[idx + 1]
    else failwith "Missing required argument: --project-path <path>"

let targetFramework =
    let idx = args |> Array.findIndex (fun a -> a = "--tfm")
    if idx >= 0 && idx + 1 < args.Length then args[idx + 1]
    else failwith "Missing required argument: --tfm <target-framework>"

let projectFullPath = Path.GetFullPath projectPath

if not (File.Exists projectFullPath) then
    failwithf "Project path not found: %s" projectFullPath

let projectDir = Path.GetDirectoryName projectFullPath

// Clean up orphaned probe directories from aborted runs (older than 1 hour)
try
    let probeBase = Path.Combine (Path.GetTempPath (), "NuAssetDeliveryProbe")
    if Directory.Exists probeBase then
        let cutoff = DateTime.UtcNow.AddHours -1.
        for dir in Directory.EnumerateDirectories probeBase do
            try
                if Directory.GetLastWriteTimeUtc dir < cutoff then
                    Directory.Delete (dir, recursive = true)
            with _ -> ()
with _ -> ()

// ---- NuGet feed query ----

let fetchVersionsFromNuget () : string[] =
    task {
        use client = new HttpClient (Timeout = TimeSpan.FromSeconds 30.)
        try
            let url = "https://api.nuget.org/v3-flatcontainer/xamarin.google.android.play.asset.delivery/index.json"
            let! json = client.GetStringAsync url
            use doc = System.Text.Json.JsonDocument.Parse json
            let root = doc.RootElement
            let mutable versionsProp = Unchecked.defaultof<_>
            if root.TryGetProperty ("versions", &versionsProp)
            then return versionsProp.EnumerateArray () |> Seq.map (fun v -> v.GetString ()) |> Seq.filter (fun v -> not (String.IsNullOrWhiteSpace v)) |> Seq.toArray
            else return [||]
        with _ -> return [||] }
    |> Async.AwaitTask
    |> Async.RunSynchronously

let fallbackVersionsFromAssets () : string[] =
    let assetsPath = Path.Combine (projectDir, "obj", "project.assets.json")
    if File.Exists assetsPath then
        let content = File.ReadAllText assetsPath
        let m = Regex.Match (content, "\"Xamarin.Google.Android.Play.Asset.Delivery/([^\"]+)\"")
        if m.Success then [| m.Groups[1].Value |]
        else [||]
    else [||]

let tryParseVersion (s: string) : Version voption =
    let clean = s.Replace("-alpha", "").Replace("-beta", "").Replace("-rc", "").Replace("-preview", "")
    let dotParts = clean.Split('.')
    // Pad to at least 4 parts
    let padded =
        [|yield! dotParts
          for _ in dotParts.Length .. 3 do yield "0"|]
        |> Array.map (fun p ->
            match Int32.TryParse p with
            | true, v -> v
            | _ -> 0)
    try
        ValueSome (Version (padded[0], padded[1], padded[2], padded[3]))
    with _ ->
        ValueNone

let runRestoreProbe (candidate: string) : bool * bool =
    // Use a temporary project file to probe — running restore on the real
    // project would overwrite obj/project.assets.json with Android-only data,
    // losing non-Android TFM entries and breaking subsequent builds.
    let probeDir = Path.Combine (Path.GetTempPath (), "NuAssetDeliveryProbe", Guid.NewGuid().ToString "N", candidate.Replace (".", "_"))
    Directory.CreateDirectory probeDir |> ignore
    let probeProject = Path.Combine (probeDir, "Probe.fsproj")

    // Create a minimal project that reproduces the same dependency chain:
    // MAUI (implicit via UseMaui=true, pulls in Ktx packages with upper bounds)
    // + Asset.Delivery (pulls in Fragment 1.8.9.2+ which violates those bounds)
    File.WriteAllText (probeProject, $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>{targetFramework}</TargetFramework>
    <UseMaui>true</UseMaui>
    <SkipValidateMauiImplicitPackageReferences>true</SkipValidateMauiImplicitPackageReferences>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Xamarin.Google.Android.Play.Asset.Delivery" Version="{candidate}" />
  </ItemGroup>
</Project>""")

    let psi = ProcessStartInfo "dotnet"
    psi.ArgumentList.Add "restore"
    psi.ArgumentList.Add probeProject
    psi.ArgumentList.Add "-p:NuGetAudit=false" // executing NuGet package vulnerability auditing is unnecessary overhead
    psi.ArgumentList.Add "-v:minimal"
    psi.ArgumentList.Add "-nologo"
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    use p = new Process ()
    p.StartInfo <- psi
    p.Start () |> ignore
    let stdout = p.StandardOutput.ReadToEnd ()
    let stderr = p.StandardError.ReadToEnd ()
    p.WaitForExit ()

    let output = stdout + stderr
    let succeeded = p.ExitCode = 0
    let hasNu1608 = output.Contains "NU1608"
    // Clean up temp directory after probe
    try Directory.Delete (probeDir, recursive = true) with _ -> ()
    succeeded, hasNu1608

// ---- Main ----

let candidates =
    let fromNuGet = fetchVersionsFromNuget ()
    if fromNuGet.Length > 0 then fromNuGet
    else fallbackVersionsFromAssets ()

if candidates.Length = 0 then
    failwith "No Xamarin.Google.Android.Play.Asset.Delivery versions found to probe."

let sorted =
    candidates
    |> Array.distinct
    |> Array.sortByDescending (fun v ->
        match tryParseVersion v with
        | ValueSome vv -> vv
        | ValueNone -> Version ())

match
    Array.tryFind (fun candidate ->
        let succeeded, hasNu1608 = runRestoreProbe candidate
        succeeded && not hasNu1608) sorted with
| Some v ->
    printf "%s" v  // no newline — MSBuild ConsoleToMsBuild trimming
| None ->
    failwithf "No compatible Xamarin.Google.Android.Play.Asset.Delivery version found that avoids NU1608. Probed %d versions: %s"
        sorted.Length (String.concat ", " sorted)
