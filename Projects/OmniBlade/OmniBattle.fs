// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharpx.Collections
open Prime
open Nu

type BattleState =
    | BattleReady of int64
    | BattleRunning
    | BattleResults of bool * int64
    | BattleCease of bool * Advent Set * int64

type [<ReferenceEquality; NoComparison>] ActionCommand =
    { Action : ActionType
      Source : CharacterIndex
      TargetOpt : CharacterIndex option }

    static member make action source targetOpt =
        { Action = action
          Source = source
          TargetOpt = targetOpt }

type [<ReferenceEquality; NoComparison>] CurrentCommand =
    { TimeStart : int64
      ActionCommand : ActionCommand }

    static member make timeStart actionCommand =
        { TimeStart = timeStart; ActionCommand = actionCommand }

[<RequireQualifiedAccess>]
module Battle =

    type [<ReferenceEquality; NoComparison>] Battle =
        private
            { BattleState_ : BattleState
              Characters_ : Map<CharacterIndex, Character>
              Inventory_ : Inventory
              PrizePool_ : PrizePool
              CurrentCommandOpt_ : CurrentCommand option
              ActionCommands_ : ActionCommand Queue
              DialogOpt_ : Dialog option }

        (* Local Properties *)
        member this.BattleState = this.BattleState_
        member this.Inventory = this.Inventory_
        member this.PrizePool = this.PrizePool_
        member this.CurrentCommandOpt = this.CurrentCommandOpt_
        member this.ActionCommands = this.ActionCommands_
        member this.DialogOpt = this.DialogOpt_

    let getCharacters battle =
        battle.Characters_

    let getAllies battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Map.ofSeq

    let getEnemies battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Map.ofSeq

    let getAlliesHealthy battle =
        getAllies battle |>
        Map.filter (fun _ character -> character.IsHealthy)

    let getAlliesWounded battle =
        getAllies battle |>
        Map.filter (fun _ character -> character.IsWounded)

    let getTargets aimType battle =
        match aimType with
        | EnemyAim _ ->
            getEnemies battle
        | AllyAim healthy ->
            if healthy
            then getAlliesHealthy battle
            else getAlliesWounded battle
        | AnyAim healthy ->
            let allies =
                if healthy
                then getAlliesHealthy battle
                else getAlliesWounded battle
            let enemies = getEnemies battle
            let characters = allies @@ enemies
            characters
        | NoAim -> Map.empty

    let getAllyIndices battle =
        getAllies battle |>
        Map.toKeyList

    let getEnemyIndices battle =
        getEnemies battle |>
        Map.toKeyList

    let getAlliesHealthyIndices battle =
        getAlliesHealthy battle |>
        Map.toKeyList

    let getAlliesWoundedIndices battle =
        getAlliesWounded battle |>
        Map.toKeyList

    let getAllyIndexRandom battle =
        let alliesHealthyIndices = getAlliesHealthyIndices battle
        List.item (Gen.random1 alliesHealthyIndices.Length) alliesHealthyIndices

    let getEnemyIndexRandom battle =
        let enemyIndices = getEnemyIndices battle
        let enemyIndex = List.item (Gen.random1 enemyIndices.Length) enemyIndices
        enemyIndex

    let addCharacter index character (battle : Battle) =
        { battle with Characters_ = Map.add index character battle.Characters_ }

    let removeCharacter index (battle : Battle) =
        { battle with Characters_ = Map.remove index battle.Characters_ }

    let updateCharactersIf predicate updater (battle : Battle) =
        { battle with Characters_ = Map.map (fun index character -> if predicate index then updater character else character) battle.Characters_ }

    let updateCharacters updater battle =
        updateCharactersIf tautology updater battle

    let updateAllies updater battle =
        updateCharactersIf (function AllyIndex _ -> true | _ -> false) updater battle

    let updateEnemies updater battle =
        updateCharactersIf (function EnemyIndex _ -> true | _ -> false) updater battle

    let tryGetCharacter characterIndex battle =
        Map.tryFind characterIndex battle.Characters_

    let getCharacter characterIndex battle =
        tryGetCharacter characterIndex battle |> Option.get

    let tryUpdateCharacter updater characterIndex battle =
        match tryGetCharacter characterIndex battle with
        | Some character ->
            let character = updater character
            { battle with Characters_ = Map.add characterIndex character battle.Characters_ }
        | None -> battle

    let updateCharacter updater characterIndex battle =
        let character = getCharacter characterIndex battle
        let character = updater character
        { battle with Characters_ = Map.add characterIndex character battle.Characters_ }

    let updateBattleState updater battle =
        { battle with BattleState_ = updater battle.BattleState_ }

    let updateInventory updater battle =
        { battle with Inventory_ = updater battle.Inventory_ }

    let updateCurrentCommandOpt updater battle =
        { battle with CurrentCommandOpt_ = updater battle.CurrentCommandOpt_ }

    let updateActionCommands updater battle =
        { battle with ActionCommands_ = updater battle.ActionCommands_ }

    let updateDialogOpt updater field =
        { field with DialogOpt_ = updater field.DialogOpt_ }

    let characterAppendedActionCommand characterIndex battle =
        seq battle.ActionCommands_ |>
        Seq.exists (fun command -> command.Source = characterIndex)

    let appendActionCommand command battle =
        { battle with ActionCommands_ = Queue.conj command battle.ActionCommands }

    let prependActionCommand command battle =
         { battle with ActionCommands_ = Queue.rev battle.ActionCommands |> Queue.conj command |> Queue.rev }

    let makeFromParty party inventory (prizePool : PrizePool) battleData time =
        let enemies = battleData.BattleEnemies |> List.mapi Character.tryMakeEnemy |> List.definitize
        let characters = party @ enemies |> Map.ofListBy (fun (character : Character) -> (character.CharacterIndex, character))
        let prizePool = { prizePool with Gold = List.fold (fun gold (enemy : Character) -> gold + enemy.GoldPrize) prizePool.Gold enemies }
        let prizePool = { prizePool with Exp = List.fold (fun exp (enemy : Character) -> exp + enemy.ExpPrize) prizePool.Exp enemies }
        let battle =
            { BattleState_ = BattleReady time
              Characters_ = characters
              Inventory_ = inventory
              PrizePool_ = prizePool
              CurrentCommandOpt_ = None
              ActionCommands_ = Queue.empty
              DialogOpt_ = None }
        battle

    let makeFromTeam (team : Team) inventory prizePool battleData time =
        let party =
            team |>
            Map.toList |>
            List.tryTake 3 |>
            List.map
                (fun (index, leg) ->
                    match Map.tryFind leg.CharacterType Data.Value.Characters with
                    | Some characterData ->
                        // TODO: bounds checking
                        let bounds = v4Bounds battleData.BattleAllyPositions.[index] Constants.Gameplay.CharacterSize
                        let characterIndex = AllyIndex index
                        let characterType = characterData.CharacterType
                        let characterState = CharacterState.make characterData leg.HitPoints leg.TechPoints leg.ExpPoints leg.WeaponOpt leg.ArmorOpt leg.Accessories
                        let animationSheet = characterData.AnimationSheet
                        let direction = Direction.ofVector2 -bounds.Bottom
                        let actionTime = Constants.Battle.AllyActionTimeInitial
                        let character = Character.make bounds characterIndex characterType characterState animationSheet direction actionTime
                        character
                    | None -> failwith ("Could not find CharacterData for '" + scstring leg.CharacterType + "'."))
        let battle = makeFromParty party inventory prizePool battleData time
        battle

    let empty =
        { BattleState_ = BattleReady 0L
          Characters_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          PrizePool_ = { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }
          CurrentCommandOpt_ = None
          ActionCommands_ = Queue.empty
          DialogOpt_ = None }

type Battle = Battle.Battle