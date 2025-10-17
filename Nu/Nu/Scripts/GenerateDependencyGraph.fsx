#r "System.Drawing"
#r "FSharp.Compiler.Service"
#r "nuget: MSBuild.StructuredLogger"
#r "nuget: GiGraph.Dot, 4.1.0"

open System
open System.IO
open System.Diagnostics
open System.Drawing
open Microsoft.Build.Logging.StructuredLogger
open FSharp.Compiler.CodeAnalysis
open GiGraph.Dot.Entities.Edges
open GiGraph.Dot.Entities.Edges.Endpoints
open GiGraph.Dot.Entities.Nodes
open GiGraph.Dot.Entities.Graphs
open GiGraph.Dot.Entities.Labels
open GiGraph.Dot.Extensions
open GiGraph.Dot.Types.Colors
open GiGraph.Dot.Types.Edges
open GiGraph.Dot.Types.Layout
open GiGraph.Dot.Types.Ranks

let private hueToRgb v1 v2 vh =
    let vh =
        if vh < 0.0 then vh + 1.0
        elif vh > 1.0 then vh - 1.0
        else vh
    if 6.0 * vh < 1.0 then v1 + (v2 - v1) * 6.0 * vh
    elif 2.0 * vh < 1.0 then v2
    elif 3.0 * vh < 2.0 then v1 + (v2 - v1) * ((2.0 / 3.0) - vh) * 6.0
    else v1

/// Convert HSL to RGB
let hslToRgb (h, s, l) =
    let r, g, b =
        if s = 0.0 then
            l, l, l
        else
            let h = h / 360.0
            let v2 =
                if l < 0.5 then l * (1.0 + s)
                else (l + s) - (l * s)
            let v1 = 2.0 * l - v2
            let r = hueToRgb v1 v2 (h + 1.0 / 3.0)
            let g = hueToRgb v1 v2 h
            let b = hueToRgb v1 v2 (h - 1.0 / 3.0)
            r, g, b
    int (255.0 * r), int (255.0 * g), int (255.0 * b)

/// Create a text file with the F# compiler arguments scrapped from a binary log file.
/// Run `dotnet build --no-incremental -bl` to create the binlog file.
/// The --no-incremental flag is essential for this scraping code.
/// Taken from https://blog.nojaf.com/2023/02/02/my-fsharp-compiler-scripts/
let makeCompilerArgsFromBinLog file =

    // read the binary log file
    let build = BinaryLog.ReadBuild file

    // find the project name
    let projectName =
        build.Children
        |> Seq.choose
            (function
             | :? Project as p -> Some p.Name
             | _ -> None)
        |> Seq.distinct
        |> Seq.exactlyOne

    // find the fsc task for that project
    let message (fscTask: FscTask) =
        fscTask.Children
        |> Seq.tryPick
            (function
             | :? Message as m when m.Text.Contains "fsc" -> Some m.Text
             | _ -> None)

    // extract the arguments
    let mutable args = None
    build.VisitAllChildren<Task>(fun task ->
        match task with
        | :? FscTask as fscTask ->
            match fscTask.Parent.Parent with
            | :? Project as p when p.Name = projectName -> args <- message fscTask
            | _ -> ()
        | _ -> ())

    // attempt to parse the arguments
    match args with
    | Some args -> args.Substring(args.IndexOf "-o:")
    | None -> failwith "Could not process the binlog file. Did you build using '--no-incremental'?"

// generate a dependency graph of the Nu project and open it in a web browser
do

    // build the project to get an up-to-date binlog file
    Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../"
    Process.Start("dotnet", "build --no-incremental -bl").WaitForExit()

    // create the graph
    let graph = DotGraph(directed = true)
    graph.Layout.Direction <- DotLayoutDirection.TopToBottom
    graph.Layout.EdgeOrderingMode <- System.Nullable(DotEdgeOrderingMode.IncomingEdges)
    graph.Subgraphs.AddWithNodes(DotRank.Same, ["Physics"; "Render"; "Audio"; "Symbolics"]) |> ignore
    
    // parse the binlog file to get the compiler arguments
    let log = makeCompilerArgsFromBinLog "msbuild.binlog"
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let projFile = __SOURCE_DIRECTORY__ + "Nu.fsproj"
    let opts = checker.GetProjectOptionsFromCommandLineArgs(projFile, log.Split('\n'))
    let checkProjectResults = checker.ParseAndCheckProject(opts) |> Async.RunSynchronously
    let uses = checkProjectResults.GetAllUsesOfAllSymbols()
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

    // create the graph nodes
    (deps |> List.map _.declarationDir) @ (deps |> List.map _.usageDir)
    |> List.distinct
    |> List.map (fun name -> graph.Nodes.Add(name, fun node ->
        let color =
            let r, g, b = hslToRgb (float (Random(name.GetHashCode()).Next(360)), 0.7, 0.8)
            Color.FromArgb(r, g, b)
        node.FillColor <- DotColorDefinition.op_Implicit(color)
        node.Label <- DotLabel.FromText name)) |> ignore

    // create the graph edges
    deps
    |> List.distinctBy (fun dep -> dep.usageDir, dep.declarationDir)
    |> List.where (fun dep -> dep.usageDir <> dep.declarationDir)
    |> List.iter (fun dep ->
        graph.Edges.Add(dep.usageDir, dep.declarationDir, fun edge ->
            let color =
                let r, g, b =
                    if dep.usageDir = "World"
                    then 96, 96, 96
                    else hslToRgb (float (Random(dep.usageDir.GetHashCode()).Next(360)), 0.5, 0.5)
                Color.FromArgb(r, g, b)
            edge.Color <- DotColorDefinition.op_Implicit(color)) |> ignore<DotEdge>)

    // save and open the graph
    let dotFilePath = "DependencyGraph.dot"
    graph.SaveToFile(dotFilePath)
    let dot = File.ReadAllText(dotFilePath)
    let url = UriBuilder ("https://dreampuf.github.io/GraphvizOnline/?engine=dot", Fragment = dot)
    ProcessStartInfo(url.Uri.AbsoluteUri, UseShellExecute = true)
    |> Process.Start
    |> ignore<Process>