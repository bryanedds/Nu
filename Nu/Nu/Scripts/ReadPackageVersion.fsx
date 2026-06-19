#!/usr/bin/env -S dotnet fsi
// ReadPackageVersion.fsx
// Extracts the resolved versions of NuGet packages from project.assets.json.
//
// Usage:
//   dotnet fsi ReadPackageVersion.fsx --assets-file <path> --package <id1> [--package <id2> ...]
//
// Outputs lines:   <id>=<version>   to stdout, one per package.
// Exits with an error if any requested package is not found.
//
// Remarks: this is the correct approach per https://learn.microsoft.com/en-us/dotnet/core/compatibility/3.1
// > If Visual Studio needs the items to populate the Dependencies node, it reads the information directly from the assets file.

open System
open System.IO
open System.Text.Json

let args = fsi.CommandLineArgs |> Array.skip 1

let assetsFile =
    let idx = args |> Array.findIndex (fun a -> a = "--assets-file" || a = "-a")
    if idx >= 0 && idx + 1 < args.Length then args[idx + 1]
    else failwith "Missing required argument: --assets-file <path>"

let packageIds =
    args
    |> Array.windowed 2
    |> Array.filter (fun pair -> pair[0] = "--package" || pair[0] = "-p")
    |> Array.map (fun pair -> pair[1])

if Array.isEmpty packageIds then
    failwith "Missing required argument: --package <id> (at least one)"

if not (File.Exists assetsFile) then
    failwithf "Assets file not found: %s" assetsFile

let json = JsonDocument.Parse(File.ReadAllText assetsFile)

// project.assets.json structure:
// {
//   "targets": {
//     "net10.0-android": {
//       "PackageName/Version": { ... }
//     }
//   }
// }
//
// We search across all TFM targets for the given packages.

let targets = json.RootElement.GetProperty "targets"

/// Find version for a package ID across all TFM targets.
let findVersion (packageId: string) =
    targets.EnumerateObject ()
    |> Seq.collect _.Value.EnumerateObject()
    |> Seq.tryPick (fun pkg ->
        let key = pkg.Name
        if String.Equals (key, packageId, StringComparison.OrdinalIgnoreCase) then
            Some ""  // package appears without a version suffix
        elif key.StartsWith (packageId + "/", StringComparison.OrdinalIgnoreCase) then
            let v = key.Substring (packageId.Length + 1)
            if v <> "" then Some v else None
        else
            None)

for packageId in packageIds do
    match findVersion packageId with
    | Some version when version <> "" ->
        // When a single package is requested, output just the version (clean for MSBuild ConsoleToMsBuild).
        // When multiple are requested, prefix with PackageId= so callers can distinguish.
        if packageIds.Length = 1 then printf "%s" version
        else printfn "%s=%s" packageId version
    | _ -> failwithf "Package '%s' not found in %s" packageId assetsFile
