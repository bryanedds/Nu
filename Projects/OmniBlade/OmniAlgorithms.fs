namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

[<RequireQualifiedAccess>]
module Algorithms =

    let LevelMax =
        32

    let ExpReqs =
        [0; 8
         15; 22; 35; 50; 75; 100
         150; 220; 350; 500; 750; 1000
         1500; 2200; 3500; 5000; 7500; 10000
         15000; 22000; 35000; 50000; 75000; 100000
         150000; 220000; 350000; 500000; 750000; 1000000]

    let ExpReqRanges =
        List.pairwise ExpReqs

    let levelToExpPointsRange level =
        ExpReqRanges |>
        List.tryItem level |>
        Option.getOrDefault (List.last ExpReqs, Int32.MaxValue)

    let levelToExpPoints level =
        fst (levelToExpPointsRange level)

    let expPointsToLevel expPoints =
        ExpReqRanges |>
        List.tryFindIndex (fun (low, high) -> expPoints >= low && expPoints < high) |>
        Option.map inc |> // level 1 is the minimum
        Option.getOrDefault LevelMax

    let hitPointsMax armorOpt archetypeType level =
        let stamina = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Stamina
            | None -> 1.0f
        let intermediate =
            match armorOpt with
            | Some armor ->
                match Map.tryFind armor data.Value.Armors with
                | Some armorData -> single armorData.HitPointsBase
                | None -> 8.0f
            | None -> 8.0f
        intermediate * single level * stamina |> int |> max 1

    let techPointsMax armorOpt archetypeType level =
        let focus = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Focus
            | None -> 1.0f
        let intermediate =
            match armorOpt with
            | Some armor ->
                match Map.tryFind armor data.Value.Armors with
                | Some armorData -> single armorData.TechPointsBase
                | None -> 4.0f
            | None -> 4.0f
        intermediate * single level * focus |> int |> max 0

    let power weaponOpt powerBuff archetypeType level =
        let strength = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Strength
            | None -> 1.0f
        let intermediate =
            match weaponOpt with
            | Some weapon ->
                match Map.tryFind weapon data.Value.Weapons with
                | Some weaponData -> single weaponData.PowerBase
                | None -> 1.0f
            | None -> 1.0f
        intermediate * single level * powerBuff * strength |> int |> max 1

    let magic weaponOpt magicBuff archetypeType level =
        let intelligence = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Intelligence
            | None -> 1.0f
        let intermediate =
            match weaponOpt with
            | Some weapon ->
                match Map.tryFind weapon data.Value.Weapons with
                | Some weaponData -> single weaponData.MagicBase
                | None -> 1.0f
            | None -> 1.0f
        intermediate * single level * magicBuff * intelligence |> int |> max 1

    let shield accessories shieldBuff archetypeType level =
        let toughness = 
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Toughness
            | None -> 1.0f
        let intermediate =
            match accessories with
            | accessory :: _ -> // just the first accessory for now
                match Map.tryFind accessory data.Value.Accessories with
                | Some weaponData -> single weaponData.ShieldBase
                | None -> 0.0f
            | _ -> 0.0f
        intermediate * single level * shieldBuff * toughness |> int |> max 0

    let techs archetypeType level =
        let techs =
            match Map.tryFind archetypeType data.Value.Archetypes with
            | Some archetypeData -> archetypeData.Techs
            | None -> Map.empty
        let indexOpt = techs |> Map.toList |> List.tryFindIndexBack (fun (levelReq, _) -> level >= levelReq)
        match indexOpt with
        | Some index -> techs |> Map.toList |> List.take (inc index) |> List.map snd |> Set.ofList
        | None -> Set.empty
        