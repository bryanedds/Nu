// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

[<RequireQualifiedAccess>]
module Algorithms =

    let expReqs =
        [0; 10; 20
         40; 80; 160; 320; 640; 1280 // 2x increase
         1920; 2880; 4320; 6480; 9720 // 1.5x increase
         12150; 15187; 18984; 23730; 29663 // 1.25x increase
         33370; 37542; 42235; 47514; 53453; 60135 // 1.125x increase
         63893; 67886; 72129; 76637; 81427; 86516] // 1.0625 increate

    let expReqRanges =
        List.pairwise expReqs

    let levelMax =
        List.length expReqs

    let levelToExpPointsRange level =
        expReqRanges |>
        List.tryItem (dec level) |> // level 1 is the minimum
        Option.getOrDefault (List.last expReqs, Int32.MaxValue)

    let levelToExpPoints level =
        fst (levelToExpPointsRange level)

    let expPointsToLevel expPoints =
        expReqRanges |>
        List.tryFindIndex (fun (low, high) -> expPoints >= low && expPoints < high) |>
        Option.map inc |> // level 1 is the minimum
        Option.getOrDefault levelMax

    let expPointsForNextLevel expPoints =
        let level = expPointsToLevel expPoints
        let (_, nextExp) = levelToExpPointsRange level
        nextExp
        
    let expPointsRemainingForNextLevel expPoints =
        match expPointsForNextLevel expPoints with
        | Int32.MaxValue -> 0
        | nextExp -> nextExp - expPoints

    let hitPointsMax armorOpt archetypeType level =
        let stamina = 
            match Map.tryFind archetypeType Data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Stamina
            | None -> 1.0f
        let intermediate =
            match armorOpt with
            | Some armor ->
                match Map.tryFind armor Data.Value.Armors with
                | Some armorData -> single armorData.HitPointsBase
                | None -> single level * 1.5f
            | None -> single level * 1.5f
        (intermediate + single level) * stamina |> int |> max 1

    let techPointsMax armorOpt archetypeType level =
        let focus = 
            match Map.tryFind archetypeType Data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Focus
            | None -> 1.0f
        let intermediate =
            match armorOpt with
            | Some armor ->
                match Map.tryFind armor Data.Value.Armors with
                | Some armorData -> single armorData.TechPointsBase
                | None -> single level
            | None -> single level
        (intermediate + single level) * focus |> int |> max 0

    let power weaponOpt powerBuff archetypeType level =
        let strength = 
            match Map.tryFind archetypeType Data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Strength
            | None -> 1.0f
        let intermediate =
            match weaponOpt with
            | Some weapon ->
                match Map.tryFind weapon Data.Value.Weapons with
                | Some weaponData -> single weaponData.PowerBase
                | None -> 1.0f
            | None -> 1.0f
        (intermediate + single level) * powerBuff * strength |> int |> max 1

    let magic weaponOpt magicBuff archetypeType level =
        let intelligence = 
            match Map.tryFind archetypeType Data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Intelligence
            | None -> 1.0f
        let intermediate =
            match weaponOpt with
            | Some weapon ->
                match Map.tryFind weapon Data.Value.Weapons with
                | Some weaponData -> single weaponData.MagicBase
                | None -> 1.0f
            | None -> 1.0f
        (intermediate + single level) * magicBuff * intelligence |> int |> max 1

    let shield effectType accessories shieldBuff archetypeType level =
        let (toughness, intelligence) = 
            match Map.tryFind archetypeType Data.Value.Archetypes with
            | Some archetypeData -> (archetypeData.Toughness, archetypeData.Intelligence)
            | None -> (1.0f, 1.0f)
        let intermediate =
            match accessories with
            | accessory :: _ -> // just the first accessory for now
                match Map.tryFind accessory Data.Value.Accessories with
                | Some accessoryData -> single accessoryData.ShieldBase
                | None -> 0.0f
            | _ -> 0.0f
        let scalar = match effectType with Magical -> intelligence * 0.5f | Physical -> toughness * 0.5f
        (intermediate + single level) * shieldBuff * scalar |> int |> max 0

    let techs archetypeType level =
        let techs =
            match Map.tryFind archetypeType Data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Techs
            | None -> Map.empty
        let indexOpt = techs |> Map.toList |> List.tryFindIndexBack (fun (levelReq, _) -> level >= levelReq)
        match indexOpt with
        | Some index -> techs |> Map.toList |> List.take (inc index) |> List.map snd |> Set.ofList
        | None -> Set.empty

    let goldPrize scalar (level : int) =
        let algo = single level * 1.5f
        int (algo * scalar)

    let expPrize scalar (level : int) =
        let algo = single level * 1.5f
        int (algo * scalar)