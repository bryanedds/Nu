// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame.Tests
open System
open System.Diagnostics
open System.IO
open System.Numerics
open NUnit.Framework
open Prime
open Nu
open ZombieGame

/// Run from the game's own output folder - the same configuration and framework as the tests' - where its assets are,
/// refined by Nu.Pipe as the game runs them.
[<SetUpFixture>]
type Setup () =
    [<OneTimeSetUp>]
    member this.SetUp () =
        let framework = DirectoryInfo TestContext.CurrentContext.TestDirectory // <tests>/bin/<configuration>/<framework>
        let configuration = framework.Parent
        let projects = configuration.Parent.Parent.Parent
        let gameDirectory = Path.Combine (projects.FullName, "Zombie Game", "bin", configuration.Name, framework.Name)
        if not (File.Exists (Path.Combine (gameDirectory, "AssetGraph.nuag"))) then failwith ("The game's not been built to " + gameDirectory + ".")
        Directory.SetCurrentDirectory gameDirectory
        Nu.init ()

/// The game itself, run headless - its game dispatcher, screens and entities in a stub world - advanced a frame at a
/// time, the player's intent given in place of the keyboard's.
[<AutoOpen>]
module Common =

    /// Make a stub world running the game, its time passing a sixtieth of a second each frame - as fast as frames can be
    /// run, rather than in real time.
    let makeWorld () =
        let worldConfig = { WorldConfig.defaultConfig with Accompanied = true; TimeAdvancing = true; TickDeltaOverride = Some (Stopwatch.Frequency / 60L) }
        World.makeStub (constant None) worldConfig (ZombieGamePlugin ())

    /// End a world - its screens destroyed first, so the entities the game's `Simulants` handles cached are invalidated
    /// rather than read by the next world's, then cleaned up.
    let endWorld world =
        for screen in World.getScreens world |> Seq.toArray do
            World.destroyScreenImmediate screen world
        World.cleanUp world

    /// Advance the world a single frame - as long as a key's press lasts, for giving intent a frame at a time, never for
    /// waiting.
    let advance (world : World) =
        let mutable advanced = false
        World.runWithoutCleanUp (fun _ -> let go = not advanced in advanced <- true; go) ignore ignore ignore ignore ignore (Some ignore) world

    /// Seconds of game time since a game time - counted by the frame rate the game runs at, so a wait means the same
    /// whatever the frame rate.
    let since (start : GameTime) (world : World) =
        (world.UnscaledTime - start).Seconds

    /// Advance the world some seconds of game time.
    let runFor seconds (world : World) =
        let start = world.UnscaledTime
        World.runWithoutCleanUp (fun world -> since start world < seconds) ignore ignore ignore ignore ignore (Some ignore) world

    /// Advance the world until a condition holds, or fail after some seconds of game time.
    let runUntil (condition : World -> bool) seconds (what : string) (world : World) =
        let start = world.UnscaledTime
        World.runWithoutCleanUp (fun world -> since start world < seconds && not (condition world)) ignore ignore ignore ignore ignore (Some ignore) world
        if not (condition world) then Assert.Fail ("Never " + what + ".")

    /// Give the player's intent from now on, in place of the keyboard's.
    let intend intent world =
        Simulants.Gameplay.SetIntentOverrideOpt (Some intent) world

    /// Run until play's going, in the area it started in.
    let private awaitPlay world =
        intend Intent.empty world
        runUntil (fun world ->
            Simulants.Gameplay.GetSelected world &&
            Simulants.GameplayPlayer.GetExists world &&
            (Simulants.Gameplay.GetPendingStartOpt world).IsNone &&
            (Simulants.Gameplay.GetArea world / "Scenery").GetExists world) 4.0 "started play" world

    /// Run until the game's screens are made, as they are once it's begun.
    let private awaitScreens world =
        runUntil (fun world -> Simulants.Title.GetExists world && Simulants.Gameplay.GetExists world) 2.0 "made the game's screens" world

    /// Start play from a start, the game's title passed, and run until it's going.
    let startFrom (start : Save) world =
        awaitScreens world
        Simulants.Gameplay.Start start world
        Game.SetGameState Playing world
        awaitPlay world

    /// Start play as one of the game's edit modes does - as the editor would - and run until it's going.
    let startEditMode name world =
        awaitScreens world
        match Map.tryFind name (ZombieGamePlugin ()).EditModes with
        | Some editMode -> editMode world
        | None -> Assert.Fail ("There's no edit mode named '" + name + "'.")
        awaitPlay world

    /// Start a new game with the fixed game seed.
    let startNewGame world =
        startFrom (Save.makeStart Constants.Gameplay.FixedGameSeed) world

    /// Start play in an area, come through its first door, with flags and an inventory.
    let startIn area flags inventory world =
        startFrom (Save.makeIn area flags inventory) world

    /// The code of the keypad a prop of an area asks, as its scene gives it.
    let getCode area name =
        let prop = Data.Areas[area].Props |> List.find (fun prop -> prop.PropName = name)
        let codeOpt =
            prop.PropInteractOpt |>
            Option.bind (fun interact -> Cue.getAll interact.Cue |> List.tryPick (function Keypad keypad -> Some keypad.Code | _ -> None))
        match codeOpt with
        | Some code -> code
        | None -> failwith ("The prop '" + name + "' asks no code.")

    /// The doors of an area, each with where it leads.
    let getDoors area =
        Data.Areas[area].Props |> List.choose (fun prop -> prop.EnterOpt |> Option.map (fun enter -> (prop, enter)))

    /// Where one stands before a door to open it, and the way one faces - where one arrives through it, turned round.
    let getDoorStand door =
        let (arrival, yaw) = Prop.getArrival door
        (Collider.toFloor arrival, yaw + MathF.PI)

    /// Hold play still - nothing acting - so the area can be looked over as it stands.
    let holdStill world =
        Simulants.Gameplay.SetMode (Reading { Lines = ["Held still for a test."]; Page = 0; After = Fin; ActedOnOpt = None }) world

    /// The entities of the area the player's in.
    let getAreaEntities world =
        Simulants.Gameplay.GetAreaEntities world |> List.ofArray

    /// Every case of a union of cases without fields.
    let cases<'u> () =
        Reflection.FSharpType.GetUnionCases typeof<'u> |>
        Array.map (fun case -> Reflection.FSharpValue.MakeUnion (case, [||]) :?> 'u) |>
        List.ofArray

    /// An inventory with a gun in hand and rounds for it.
    let Armed =
        Inventory.Initial |> Inventory.add Handgun 1 |> fst |> Inventory.add HandgunBullets 45 |> fst |> Inventory.equip Handgun
