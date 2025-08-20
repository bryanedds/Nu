#r "FSharp.Compiler.Service"
#r "nuget: MSBuild.StructuredLogger"
#r "System.Drawing"
#r "nuget: GiGraph.Dot, 4.1.0"

open System.IO
open System.Drawing
open GiGraph.Dot.Entities.Edges
open GiGraph.Dot.Entities.Labels
open GiGraph.Dot.Entities.Edges.Endpoints
open GiGraph.Dot.Entities.Nodes
open GiGraph.Dot.Types.Edges
open GiGraph.Dot.Types.Colors
open GiGraph.Dot.Types.Layout
open GiGraph.Dot.Types.Ranks
open Microsoft.Build.Logging.StructuredLogger
open GiGraph.Dot.Entities.Graphs
open GiGraph.Dot.Extensions
open FSharp.Compiler.CodeAnalysis

let hueToRgb v1 v2 vh =
    let vh =
        if vh < 0. then vh + 1.
        elif vh > 1. then vh - 1.
        else vh
    if 6. * vh < 1. then v1 + (v2 - v1) * 6. * vh
    elif 2. * vh < 1. then v2
    elif 3. * vh < 2. then v1 + (v2 - v1) * ((2. / 3.) - vh) * 6.
    else v1

let hslToRgb (h, s, l) =
    let r, g, b =
        if s = 0. then
            l, l, l
        else
            let h = h / 360.
            let v2 =
                if l < 0.5 then l * (1. + s)
                else (l + s) - (l * s)
            let v1 = 2. * l - v2
            let r = hueToRgb v1 v2 (h + 1./3.)
            let g = hueToRgb v1 v2 h
            let b = hueToRgb v1 v2 (h - 1./3.)
            r, g, b
    int (255. * r), int (255. * g), int (255. * b)

/// Create a text file with the F# compiler arguments scrapped from a binary log file.
/// Run `dotnet build --no-incremental -bl` to create the binlog file.
/// The --no-incremental flag is essential for this scraping code.
/// Taken from https://blog.nojaf.com/2023/02/02/my-fsharp-compiler-scripts/
let mkCompilerArgsFromBinLog file =
    let build = BinaryLog.ReadBuild file

    let projectName =
        build.Children
        |> Seq.choose
            (function
             | :? Project as p -> Some p.Name
             | _ -> None)
        |> Seq.distinct
        |> Seq.exactlyOne

    let message (fscTask: FscTask) =
        fscTask.Children
        |> Seq.tryPick
            (function
             | :? Message as m when m.Text.Contains "fsc" -> Some m.Text
             | _ -> None)

    let mutable args = None

    build.VisitAllChildren<Task>(fun task ->
        match task with
        | :? FscTask as fscTask ->
            match fscTask.Parent.Parent with
            | :? Project as p when p.Name = projectName -> args <- message fscTask
            | _ -> ()
        | _ -> ())

    match args with
    | None -> failwith "Could not process the binlog file. Did you build using '--no-incremental'?"
    | Some args ->
        let content =
            let idx = args.IndexOf "-o:"
            args.Substring(idx)
        content
        
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../"

System.Diagnostics.Process.Start("dotnet", "build --no-incremental -bl").WaitForExit()

let log = mkCompilerArgsFromBinLog "msbuild.binlog"

let checker = FSharpChecker.Create(keepAssemblyContents = true)

let projFile = __SOURCE_DIRECTORY__ + "Nu.fsproj"

let opts = checker.GetProjectOptionsFromCommandLineArgs(projFile, log.Split('\n'))

let checkProjectResults =
    checker.ParseAndCheckProject(opts)
    |> Async.RunSynchronously
  
let uses = checkProjectResults.GetAllUsesOfAllSymbols()

System.Environment.CurrentDirectory

do
    let graph = DotGraph(directed = true)
    graph.Layout.Direction <- DotLayoutDirection.TopToBottom
    graph.Layout.EdgeOrderingMode <- System.Nullable(DotEdgeOrderingMode.IncomingEdges)

    graph.Subgraphs.AddWithNodes(DotRank.Same, ["Physics"; "Render"; "Audio"]) |> ignore
    graph.Subgraphs.AddWithNodes(DotRank.Same, ["ImGui"; "Reflection"; "Assimp"]) |> ignore

    let deps =
        [for usage in uses do
            if usage.Symbol.Assembly.SimpleName = "Nu" then
                 match usage.Symbol.DeclarationLocation with
                 | Some location ->
                     if usage.FileName <> location.FileName then
                         let usageDir = Path.GetDirectoryName(usage.FileName)
                         let declarationDir = Path.GetDirectoryName(location.FileName)
                         if  declarationDir <> "Core" &&
                             usageDir <> "" && declarationDir <> "" &&
                             usageDir <> declarationDir &&
                             not usage.IsFromOpenStatement then
                             {|usage = usage.FileName
                               declaration = location.FileName
                               usageDir = usageDir
                               declarationDir = declarationDir|}
                      else ()
                  | None -> ()]

    (deps |> List.map _.declarationDir) @ (deps |> List.map _.usageDir)
    |> List.distinct
    |> List.map (fun name -> graph.Nodes.Add(name, fun node ->
        let color =
            let r, g, b = hslToRgb (float (System.Random(name.GetHashCode()).Next(360)), 0.7, 0.8)
            Color.FromArgb(r, g, b)
        node.FillColor <- DotColorDefinition.op_Implicit(color)
        node.Label <- DotLabel.FromText name)) |> ignore

    deps
    |> List.distinctBy (fun dep -> dep.usageDir, dep.declarationDir)
    |> List.where (fun dep -> dep.usageDir <> dep.declarationDir)
    |> List.iter (fun dep ->
        graph.Edges.Add(dep.usageDir, dep.declarationDir, fun edge ->
            let color =
                let r, g, b =
                    if dep.usageDir = "World"
                    then 96, 96, 96
                    else hslToRgb (float (System.Random(dep.usageDir.GetHashCode()).Next(360)), 0.5, 0.5)
                Color.FromArgb(r, g, b)
            edge.Color <- DotColorDefinition.op_Implicit(color)) |> ignore<DotEdge>)

    graph.SaveToFile("Scripts/deps.dot")
