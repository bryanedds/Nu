// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.IO
open System.Numerics
open Prime
open Nu

/// The game's data, read once from its sheets, scene files, appearances files and puppet files - by key where it's
/// looked up, in order where it's listed.
[<RequireQualifiedAccess>]
module Data =

    /// Read a sheet's rows, in order.
    let private read<'d> filePath : 'd list =
        Numerics.Math.Init () // a sheet's cells may hold Nu's math types, as colors and vectors, whose converters this assigns
        let text = File.ReadAllText filePath
        match Symbol.ofStringCsv true text (Some filePath) with
        | Symbols (rows, _) ->
            List.mapi (fun i row ->
                try symbolToValue<'d> row
                with exn -> failwith ("Could not read row " + string (i + 2) + " of '" + filePath + "' - " + exn.Message))
                rows
        | _ -> failwith ("Could not read the rows of '" + filePath + "'.")

    /// Read a symbol file - a single value in Prime's symbol syntax, as a scene file is.
    let private readSymbolFile<'d> filePath : 'd =
        Numerics.Math.Init ()
        let text = File.ReadAllText filePath
        try symbolToValue<'d> (Symbol.ofString text (Some filePath))
        with exn -> failwith ("Could not read '" + filePath + "' - " + exn.Message)

    /// Read a sheet's rows into a map by their keys, which must each be on one row only.
    let private readMap<'d, 'k when 'k : comparison> filePath (getKey : 'd -> 'k) =
        read<'d> filePath |>
        List.fold (fun map data ->
            let key = getKey data
            if Map.containsKey key map then failwith ("Key '" + scstring key + "' is on more than one row of '" + filePath + "'.")
            Map.add key data map)
            Map.empty

    /// The items the player can carry, by item type.
    let Items = readMap Assets.Data.ItemDataFilePath (fun (data : ItemData) -> data.Item)

    /// How each weapon among the items fights.
    let Weapons = Items |> Map.toSeq |> Seq.choose (fun (item, data) -> match data.ItemKind with Weapon weapon -> Some (item, weapon) | _ -> None) |> Map.ofSeq

    /// The items that combine, each pair either way round.
    let Combinations = read<CombinationData> Assets.Data.CombinationDataFilePath

    /// What the player sets out with.
    let StartItems = read<StartItemData> Assets.Data.StartItemDataFilePath

    /// The creatures the player fights, by creature type.
    let Creatures = readMap Assets.Data.CreatureDataFilePath (fun (data : CreatureData) -> data.CreatureType)

    /// How each surface takes the light.
    let Surfaces = readMap Assets.Data.SurfaceDataFilePath (fun (data : SurfaceData) -> data.Surface)

    /// The credits, in order.
    let Credits = read<CreditData> Assets.Data.CreditDataFilePath

    /// Every scene, from its scene file, by name.
    let Scenes =
        Directory.EnumerateFiles (Assets.Gameplay.SceneDirPath, "*.scene") |>
        Seq.map (fun filePath ->
            let name = Path.GetFileNameWithoutExtension filePath
            (name, { SceneName = name; Statements = readSymbolFile<SceneStatement list> filePath })) |>
        Map.ofSeq

    /// Every appearances file's statements, from the appearances files with the scene files.
    let AppearancesStatements =
        Directory.EnumerateFiles (Assets.Gameplay.SceneDirPath, "*.appearances") |>
        Seq.sortBy id |>
        Seq.collect (fun filePath -> readSymbolFile<AppearancesStatement list> filePath |> Seq.map (fun statement -> (filePath, statement))) |>
        List.ofSeq

    /// The variants of appearances the appearances files define, by name.
    let Variants =
        AppearancesStatements |>
        List.fold (fun variants (filePath, statement) ->
            match statement with
            | Define (name, appearance) ->
                if Map.containsKey name variants then failwith ("The variant '" + name + "' is defined more than once, again in '" + filePath + "'.")
                Map.add name appearance variants
            | SharedPalette _ | Compose _ -> variants)
            Map.empty

    /// The appearances the appearances files compose of parts, by name.
    let ComposedAppearances =
        AppearancesStatements |>
        List.fold (fun appearances (filePath, statement) ->
            match statement with
            | Compose (name, composed) ->
                if Map.containsKey name appearances then failwith ("The composed appearance '" + name + "' is composed more than once, again in '" + filePath + "'.")
                Map.add name composed appearances
            | Define _ | SharedPalette _ -> appearances)
            Map.empty

    /// The colors the appearances files give every scene to name, by name - a scene's own palette taking precedence.
    let SharedPalette =
        AppearancesStatements |>
        List.fold (fun palette (filePath, statement) ->
            match statement with
            | SharedPalette colors ->
                colors |>
                Map.fold (fun palette name color ->
                    if Map.containsKey name palette then failwith ("The shared color '" + name + "' is given more than once, again in '" + filePath + "'.")
                    Map.add name color palette)
                    palette
            | Define _ | Compose _ -> palette)
            Map.empty

    /// Every body plan's pose set, from its pose file, named for its PoseSetType case.
    let PoseSets =
        Directory.EnumerateFiles (Assets.Gameplay.PuppetDirPath, "*.poses") |>
        Seq.map (fun filePath ->
            let poseSetType = scvalue<PoseSetType> (Path.GetFileNameWithoutExtension filePath)
            (poseSetType, readSymbolFile<PoseStatement list> filePath)) |>
        Map.ofSeq

    /// Every body plan's animation set, from its animation file, named for its AnimationSetType case.
    let AnimationSets =
        Directory.EnumerateFiles (Assets.Gameplay.PuppetDirPath, "*.animations") |>
        Seq.map (fun filePath ->
            let animationSetType = scvalue<AnimationSetType> (Path.GetFileNameWithoutExtension filePath)
            (animationSetType, readSymbolFile<AnimationStatement list> filePath)) |>
        Map.ofSeq

    /// The scenes and the appearances files' tables, as building a prop of an appearance and resolving a scene read
    /// them.
    let AppearancesLibrary =
        { Scenes = Scenes
          Variants = Variants
          ComposedAppearances = ComposedAppearances
          SharedPalette = SharedPalette }

    /// Every area, resolved from its scene.
    let Areas =
        Scenes |>
        Map.toList |>
        List.choose (fun (_, scene) ->
            scene.Statements |>
            List.tryPick (function Area area -> Some (area.Type, scene) | Palette _ | Layout _ | Prop _ | Enemy _ | Include _ | Arrival _ | Camera _ | While _ -> None)) |>
        List.groupBy fst |>
        List.map (fun (areaType, scenes) ->
            match scenes with
            | [(_, scene)] -> (areaType, Resolver.resolve Constants.Gameplay.FixedGameSeed scene AppearancesLibrary)
            | _ -> failwith ("More than one scene is the area " + scstring areaType + " - " + String.Join (", ", scenes |> List.map (fun (_, scene) -> scene.SceneName)) + ".")) |>
        Map.ofList

    /// The colliders of each area that stand taller than a creature's eyes, so it can't see past them.
    let SightBlockers = Areas |> Map.map (fun _ area -> Area.getSightBlockers area)

    /// Where one comes into each area - at its first arrival, or else through the first of its doors - and the way one
    /// faces there.
    let Entrances = Areas |> Map.map (fun _ area -> Area.getEntrance area)

    /// The area play starts in - the one with the arrival named "Start".
    let StartArea =
        match Areas |> Map.filter (fun _ area -> area.Arrivals |> List.exists (fun arrival -> arrival.Name = "Start")) |> Map.toList with
        | [(areaType, _)] -> areaType
        | areas -> failwith ("One area should have the arrival named 'Start', not " + string areas.Length + ".")

    /// Where play starts in its area, and the way one faces there.
    let Start = Area.getArrival "Start" Areas[StartArea]

    /// Every character's puppet, made from its puppet file, named for the character, and its body plan's pose and
    /// animation sets.
    let Puppets =
        Directory.EnumerateFiles (Assets.Gameplay.PuppetDirPath, "*.puppet") |>
        Seq.map (fun filePath ->
            let name = Path.GetFileNameWithoutExtension filePath
            let characterType = if name = nameof Player then Player else Creature (scvalue<CreatureType> name)
            (characterType, Puppet.make characterType.Name (readSymbolFile<PuppetData> filePath) PoseSets AnimationSets Surfaces)) |>
        Map.ofSeq