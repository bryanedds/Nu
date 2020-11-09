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
        [0; 7; 14
         30; 60; 120; 250; 500; 1000 // 2x increase
         1500; 2250; 3375; 5000; 7500 // 1.5x increase
         9375; 11700; 14500; 18000; 22500 // 1.25x increase
         25000; 28500; 32000; 36000; 40500; 45500 // 1.125x increase
         48350; 51350; 54575; 5800; 61600; 65500] // 1.0625 increate

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
                | None -> 4.0f
            | None -> 4.0f
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
                | None -> 4.0f
            | None -> 4.0f
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
        let scalar = match effectType with Magical -> intelligence | Physical -> toughness
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
        let algo = level * 2
        int (single algo * scalar)

    let expPrize scalar (level : int) =
        let algo = level
        int (single algo * scalar)