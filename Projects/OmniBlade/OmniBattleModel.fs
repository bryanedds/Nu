namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type [<StructuralEquality; StructuralComparison>] BattleState =
    | BattleReady of int64
    | BattleRunning
    | BattleCease of bool * int64

type [<StructuralEquality; NoComparison>] ActionCommand =
    { Action : ActionType
      Source : CharacterIndex
      TargetOpt : CharacterIndex option }

    static member make action source targetOpt =
        { Action = action
          Source = source
          TargetOpt = targetOpt }

type [<StructuralEquality; NoComparison>] CurrentCommand =
    { TimeStart : int64
      ActionCommand : ActionCommand }

    static member make timeStart actionCommand =
        { TimeStart = timeStart; ActionCommand = actionCommand }

[<RequireQualifiedAccess>]
module BattleModel =

    type [<ReferenceEquality; NoComparison>] BattleModel =
        private
            { BattleState_ : BattleState
              Characters_ : Map<CharacterIndex, CharacterModel>
              Inventory_ : Inventory
              BonusItemOpt_ : ItemType option
              CurrentCommandOpt_ : CurrentCommand option
              ActionCommands_ : ActionCommand Queue }

        (* Local Properties *)
        member this.BattleState = this.BattleState_
        member this.Inventory = this.Inventory_
        member this.CurrentCommandOpt = this.CurrentCommandOpt_
        member this.ActionCommands = this.ActionCommands_

    let getAllies model =
        model.Characters_ |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

    let getEnemies model =
        model.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

    let getAlliesHealthy model =
        getAllies model |>
        List.filter (fun character -> character.IsHealthy)

    let getAlliesWounded model =
        getAllies model |>
        List.filter (fun character -> character.IsWounded)

    let getAllyIndices model =
        getAllies model |>
        List.map (fun ally -> ally.CharacterIndex)

    let getEnemyIndices model =
        getEnemies model |>
        List.map (fun enemy -> enemy.CharacterIndex)

    let getAlliesHealthyIndices model =
        getAlliesHealthy model |>
        List.map (fun ally -> ally.CharacterIndex)

    let getAlliesWoundedIndices model =
        getAlliesWounded model |>
        List.map (fun enemy -> enemy.CharacterIndex)

    let getAllyIndexRandom model =
        let alliesHealthyIndices = getAlliesHealthyIndices model
        List.item (Gen.random1 alliesHealthyIndices.Length) alliesHealthyIndices

    let getEnemyIndexRandom model =
        let enemyIndices = getEnemyIndices model
        let enemyIndex = List.item (Gen.random1 enemyIndices.Length) enemyIndices
        enemyIndex

    let getTargets aimType model =
        match aimType with
        | EnemyAim _ ->
            getEnemies model
        | AllyAim healthy ->
            if healthy
            then getAlliesHealthy model
            else getAlliesWounded model
        | AnyAim healthy ->
            let allies =
                if healthy
                then getAlliesHealthy model
                else getAlliesWounded model
            let enemies = getEnemies model
            let characters = allies @ enemies
            characters
        | NoAim -> []

    let addCharacter index character (model : BattleModel) =
        { model with Characters_ = Map.add index character model.Characters_ }

    let removeCharacter index (model : BattleModel) =
        { model with Characters_ = Map.remove index model.Characters_ }

    let updateCharactersIf predicate updater (model : BattleModel) =
        { model with Characters_ = Map.map (fun index character -> if predicate index then updater character else character) model.Characters_ }

    let updateCharacters updater model =
        updateCharactersIf tautology updater model

    let updateAllies updater model =
        updateCharactersIf (function AllyIndex _ -> true | _ -> false) updater model

    let updateEnemies updater model =
        updateCharactersIf (function EnemyIndex _ -> true | _ -> false) updater model

    let getCharacters model =
        model.Characters_ |> Map.toValueList

    let tryGetCharacter characterIndex model =
        Map.tryFind characterIndex model.Characters_

    let getCharacter characterIndex model =
        tryGetCharacter characterIndex model |> Option.get

    let tryUpdateCharacter updater characterIndex model =
        match tryGetCharacter characterIndex model with
        | Some character ->
            let character = updater character
            { model with Characters_ = Map.add characterIndex character model.Characters_ }
        | None -> model

    let updateCharacter updater characterIndex model =
        let character = getCharacter characterIndex model
        let character = updater character
        { model with Characters_ = Map.add characterIndex character model.Characters_ }

    let updateBattleState updater model =
        { model with BattleState_ = updater model.BattleState_ }

    let updateInventory updater model =
        { model with Inventory_ = updater model.Inventory_ }

    let updateCurrentCommandOpt updater model =
        { model with CurrentCommandOpt_ = updater model.CurrentCommandOpt_ }

    let updateActionCommands updater model =
        { model with ActionCommands_ = updater model.ActionCommands_ }

    let appendActionCommand command model =
        { model with ActionCommands_ = Queue.conj command model.ActionCommands }

    let prependActionCommand command model =
         { model with ActionCommands_ = Queue.rev model.ActionCommands |> Queue.conj command |> Queue.rev }

    let make battleData allies inventory bonusItemOpt time =
        let enemies = List.mapi CharacterModel.makeEnemy battleData.BattleEnemies
        let characters = allies @ enemies |> Map.ofListBy (fun (character : CharacterModel) -> (character.CharacterIndex, character))
        let model =
            { BattleState_ = BattleReady time
              Characters_ = characters
              Inventory_ = inventory
              BonusItemOpt_ = bonusItemOpt
              CurrentCommandOpt_ = None
              ActionCommands_ = Queue.empty }
        model

    let empty =
        { BattleState_ = BattleReady 0L
          Characters_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          BonusItemOpt_ = None
          CurrentCommandOpt_ = None
          ActionCommands_ = Queue.empty }

type BattleModel = BattleModel.BattleModel