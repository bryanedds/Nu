// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame.Tests
open System
open System.IO
open System.Text
open FSharp.Compiler.Interactive
open NUnit.Framework
open Prime
open Nu
open ZombieGame

/// The game's code reloaded mid-play as Gaia's Reload All does it - the game project's source compiled afresh in an F#
/// interactive session, and the world's late bindings pointed at the new types - so what a reload does to play can be
/// seen headless.
module ReloadTests =

    let private fsiArgs = [|"fsi.exe"; "--debug+"; "--debug:full"; "--define:DEBUG"; "--optimize-"; "--tailcalls-"; "--multiemit+"; "--gui-"; "--nologo"; "--warnon:FS1182"; "--nowarn:FS0009;FS0052;FS1178;FS3391;FS3536;FS3560"|]
    let private fsiErrorStream = new StringWriter ()
    let private fsiOutStream = new StringWriter ()

    /// Reload the game's code as Gaia does - every assembly the game's output folder holds referenced, rather than
    /// Gaia's nuget packages, then every source file of the game's project but Program.fs loaded in order - and update
    /// the world's late bindings to the new assemblies.
    let reloadCode initializing (world : World) =
        let gameDir = Directory.GetCurrentDirectory () // the game's output folder, as the tests' setup sets it
        let projectDir = Path.GetFullPath (Path.Combine (gameDir, "../../.."))
        let fsprojFilePath = Directory.EnumerateFiles (projectDir, "*.fsproj") |> Seq.head
        let fsprojFileLines = File.ReadAllLines fsprojFilePath |> Array.map (fun line -> line.Trim ())
        let fsFilePaths =
            fsprojFileLines
            |> Array.filter (fun line -> line.Contains "Compile Include" && line.Contains ".fs" && not (line.Contains "Program.fs"))
            |> Array.map (fun line -> line.Replace("<Compile Include", "").Replace("/>", "").Replace("=", "").Replace("\"", "").Trim ())
            |> Array.map (fun path -> Path.GetFullPath (Path.Combine (projectDir, path.Replace ('\\', '/'))))
        let defineConstants =
            fsprojFileLines
            |> Array.filter (fun line -> line.Contains "DefineConstants")
            |> Array.map (fun line -> line.Replace("DefineConstants", "").Replace("/", "").Replace(">", "").Replace("<", ""))
            |> String.concat ";"
        let gameAssemblyName = Path.GetFileNameWithoutExtension fsprojFilePath + ".dll"
        let dllFilePaths =
            Directory.EnumerateFiles (gameDir, "*.dll")
            |> Seq.filter (fun path -> Path.GetFileName path <> gameAssemblyName)
            |> Seq.filter (fun path -> try Reflection.AssemblyName.GetAssemblyName path |> ignore; true with _ -> false) // managed only
            |> Array.ofSeq
        let fsxFileString =
            String.Join ("\n", dllFilePaths |> Array.map (fun path -> "#r \"" + path + "\"")) + "\n" +
            String.Join ("\n", fsFilePaths |> Array.map (fun path -> "#load \"" + path + "\""))
        let args = if String.IsNullOrEmpty defineConstants then fsiArgs else Array.append fsiArgs [|"--define:" + defineConstants|]
        let session = Shell.FsiEvaluationSession.Create (Shell.FsiEvaluationSession.GetDefaultConfiguration (), args, new StringReader "", fsiOutStream, fsiErrorStream)
        match session.EvalInteractionNonThrowing fsxFileString with
        | (Choice1Of2 _, _) -> World.updateLateBindings initializing session.DynamicAssemblies world
        | (Choice2Of2 exn, diags) ->
            Assert.Fail ("The game's code didn't compile for reloading: " + String.Join ("\n", diags |> Array.map string) + "\n" + string exn)
        session

    /// Reload the game's assets as Gaia does, from the game's project folder into its output folder.
    let reloadAssets (world : World) =
        let gameDir = Directory.GetCurrentDirectory ()
        let projectDir = Path.GetFullPath (Path.Combine (gameDir, "../../.."))
        match World.tryReloadAssetGraph projectDir gameDir Constants.Engine.RefinementDir Constants.Render.TextureBlockCompression world with
        | Right _ -> ()
        | Left error -> Assert.Fail ("The game's assets didn't reload: " + error)

    /// The assembly of an area type a scenery's told it's of, as of a change to it - the area's type, not the option's.
    let private getAreaTypeAssembly (value : obj) =
        match value with
        | null -> "none"
        | _ ->
            let ty = value.GetType ()
            let ty = if ty.IsGenericType then ty.GetGenericArguments().[0] else ty
            ty.Assembly.GetName().Name + " " + string (ty.Assembly.GetHashCode ())

    /// Play a new game a little, reload the game's assets and code as Gaia does - initializing or not - and play on a
    /// second, checking the scenery of the area the player's in is built no more than once, and that the ImSim flags
    /// the reload raises are down again after a frame.
    let private playThroughReload initializing =
        let world = makeWorld ()
        try startNewGame world
            let scenery = Simulants.Gameplay.GetArea world / "Scenery"
            let mutable builds = []
            World.subscribeLambda (fun evt _ ->
                builds <- (string world.UpdateTime, getAreaTypeAssembly evt.Data.Previous + " -> " + getAreaTypeAssembly evt.Data.Value + " " + scstring evt.Data.Value) :: builds
                Cascade) scenery.AreaTypeOpt.ChangeEvent scenery world
            for _ in 0 .. 9 do advance world
            reloadAssets world
            use session = reloadCode initializing world
            advance world
            let flagsAfter = (world.ImSimInitializing, world.ImSimReinitializing)
            for _ in 0 .. 58 do advance world
            for (frame, change) in List.rev builds do TestContext.WriteLine ("update " + frame + ": " + change)
            Assert.AreEqual ((false, false), flagsAfter, "ImSim's initializing and reinitializing flags were still up a frame after the reload.")
            Assert.LessOrEqual (builds.Length, 1, "The scenery was built " + string builds.Length + " times after a reload.")
        finally endWorld world

    let [<Test>] ``Reloading the game's assets and code mid-play, as Gaia's Reload All does, builds an area's scenery no more than once.`` () =
        playThroughReload false

    let [<Test>] ``Reloading and initializing mid-play, as Gaia's Reload All and Initialize does, builds an area's scenery no more than once.`` () =
        playThroughReload true
