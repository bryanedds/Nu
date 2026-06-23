/// Reads a .deps.json file and outputs #r "nuget:" directives for each NuGet dependency
/// found in the "targets" section. Only real NuGet packages (type "package" in the libraries
/// section) are included; project references and local DLL references are excluded.
/// Usage: dotnet fsi ParseDepsJson.fsx <path-to-deps.json>
open System.IO
open System.Text.RegularExpressions

let path = fsi.CommandLineArgs.[1]
let lines = File.ReadAllLines path |> Array.map (fun l -> l.TrimStart())
let pkgRex = Regex ("""^"([^"/]+)/([^"/]+)"\s*:\s*\{""", RegexOptions.Compiled)
let typeRex = Regex ("""^"type"\s*:\s*"([^"]+)"\s*,""", RegexOptions.Compiled)
let targetsRex = Regex ("""^"targets"\s*:""", RegexOptions.Compiled)
let librariesRex = Regex ("""^"libraries"\s*:""", RegexOptions.Compiled)

// First pass: collect real NuGet package IDs from the "libraries" section.
let realPackages = System.Collections.Generic.HashSet<string> ()
let mutable inLibraries = false
let mutable currentPkgName = ""

for line in lines do
    if librariesRex.IsMatch line then
        inLibraries <- true
    elif inLibraries then
        let pm = pkgRex.Match line
        if pm.Success then
            currentPkgName <- pm.Groups.[1].Value
        else
            let tm = typeRex.Match line
            if tm.Success && tm.Groups.[1].Value = "package" && currentPkgName <> "" then
                realPackages.Add currentPkgName |> ignore
                currentPkgName <- ""
            elif line.StartsWith "}" then
                currentPkgName <- ""

// Second pass: output #r "nuget:" for real packages in the "targets" section.
let mutable inTargets = false

for line in lines do
    if targetsRex.IsMatch line then
        inTargets <- true
    elif librariesRex.IsMatch line then
        inTargets <- false
    elif inTargets then
        let m = pkgRex.Match line
        if m.Success then
            let pkgName = m.Groups.[1].Value
            if not (pkgName = "FSharp.Core" || pkgName = "Nu" || pkgName = "Nu.Math" || pkgName = "Nu.Spine") && realPackages.Contains pkgName then
                let version = m.Groups.[2].Value
                printfn "#r \"nuget: %s, %s\"" pkgName version
