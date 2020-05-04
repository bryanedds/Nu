namespace OmniBlade
open FSharpx.Collections
open Prime
open Nu

type ActionCommand =
    { Action : ActionType
      Source : CharacterIndex
      TargetOpt : CharacterIndex option }

    static member make action source targetOpt =
        { Action = action
          Source = source
          TargetOpt = targetOpt }

type CurrentCommand =
    { TimeStart : int64
      ActionCommand : ActionCommand }

    static member make timeStart actionCommand =
        { TimeStart = timeStart; ActionCommand = actionCommand }

type BattleState =
    | BattleReady of int64
    | BattleRunning
    | BattleCease of bool * int64

module BattleModel =

    type [<ReferenceEquality; NoComparison>] BattleModel =
        private
            { BattleState_ : BattleState
              Characters : Map<CharacterIndex, CharacterModel>
              CurrentCommandOpt_ : CurrentCommand option
              ActionCommands_ : ActionCommand Queue
              Inventory_ : Inventory
              Gold_ : int }

        member this.BattleState = this.BattleState_
        member this.CurrentCommandOpt = this.CurrentCommandOpt_
        member this.ActionCommands = this.ActionCommands_
        member this.Inventory = this.Inventory_
        member this.Gold = this.Gold_

        static member getAllies model =
            model.Characters |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

        static member getEnemies model =
            model.Characters |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

        static member getAllyIndices model =
            model.Characters |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map fst |> Seq.toList

        static member getEnemyIndices model =
            model.Characters |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Seq.map fst |> Seq.toList

        static member getAllyIndexRandom model =
            let allyIndices = BattleModel.getAllyIndices model
            let allyIndex = List.item (Gen.random1 allyIndices.Length) allyIndices
            allyIndex

        static member getEnemyIndexRandom model =
            let enemyIndices = BattleModel.getEnemyIndices model
            let enemyIndex = List.item (Gen.random1 enemyIndices.Length) enemyIndices
            enemyIndex

        static member getAlliesHealthy model =
            BattleModel.getAllies model |>
            List.filter (fun character -> character.IsHealthy)

        static member getAlliesWounded model =
            BattleModel.getAllies model |>
            List.filter (fun character -> character.IsWounded)

        static member getTargets aimType model =
            match aimType with
            | EnemyAim -> BattleModel.getEnemies model
            | AllyAimHealthy -> BattleModel.getAlliesHealthy model
            | AllyAimWounded -> BattleModel.getAlliesWounded model
            | AnyAim -> Map.toValueList model.Characters
            | NoAim -> []

        static member addCharacter index character (model : BattleModel) =
            { model with Characters = Map.add index character model.Characters }

        static member removeCharacter index (model : BattleModel) =
            { model with Characters = Map.remove index model.Characters }

        static member updateCharactersIf predicate updater model =
            { model with BattleModel.Characters = Map.map (fun index character -> if predicate index then updater character else character) model.Characters }

        static member updateCharacters updater model =
            BattleModel.updateCharactersIf tautology updater model

        static member updateAllies updater model =
            BattleModel.updateCharactersIf (function AllyIndex _ -> true | _ -> false) updater model

        static member updateEnemies updater model =
            BattleModel.updateCharactersIf (function EnemyIndex _ -> true | _ -> false) updater model

        static member tryGetCharacter characterIndex model =
            Map.tryFind characterIndex model.Characters

        static member getCharacter characterIndex model =
            BattleModel.tryGetCharacter characterIndex model |> Option.get

        static member tryUpdateCharacter updater characterIndex model =
            match BattleModel.tryGetCharacter characterIndex model with
            | Some character ->
                let character = updater character
                { model with Characters = Map.add characterIndex character model.Characters }
            | None -> model

        static member updateCharacter updater characterIndex model =
            let character = BattleModel.getCharacter characterIndex model
            let character = updater character
            { model with Characters = Map.add characterIndex character model.Characters }

        static member updateBattleState updater model =
            { model with BattleState_ = updater model.BattleState_ }

        static member updateCurrentCommandOpt updater model =
            { model with CurrentCommandOpt_ = updater model.CurrentCommandOpt_ }

        static member updateActionCommands updater model =
            { model with ActionCommands_ = updater model.ActionCommands_ }

        static member updateInventory updater model =
            { model with Inventory_ = updater model.Inventory_ }

        static member conjActionCommand command model =
            { model with ActionCommands_ = Queue.conj command model.ActionCommands }

        static member make battleState characters currentCommandOpt actionCommands inventory gold =
            { BattleState_ = battleState
              Characters = characters
              CurrentCommandOpt_ = currentCommandOpt
              ActionCommands_ = actionCommands
              Inventory_ = inventory
              Gold_ = gold }

        static member empty =
            BattleModel.make (BattleReady 0L) Map.empty None Queue.empty { Items = Map.empty } 0

type BattleModel = BattleModel.BattleModel