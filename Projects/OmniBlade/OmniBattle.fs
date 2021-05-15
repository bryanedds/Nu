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
              TileMap_ : TileMap AssetTag
              BattleSongOpt_ : Song AssetTag option
              CurrentCommandOpt_ : CurrentCommand option
              ActionCommands_ : ActionCommand Queue
              DialogOpt_ : Dialog option }

        (* Local Properties *)
        member this.Running = match this.BattleState with BattleRunning _ -> true | _ -> false
        member this.NotRunning = not this.Running
        member this.BattleState = this.BattleState_
        member this.Inventory = this.Inventory_
        member this.PrizePool = this.PrizePool_
        member this.TileMap = this.TileMap_
        member this.BattleSongOpt = this.BattleSongOpt_
        member this.CurrentCommandOpt = this.CurrentCommandOpt_
        member this.ActionCommands = this.ActionCommands_
        member this.DialogOpt = this.DialogOpt_

    let getCharacters battle =
        battle.Characters_

    let getCharactersIf pred battle =
        battle.Characters_|>
        Map.filter pred

    let getCharactersHealthy battle =
        getCharacters battle |>
        Map.filter (fun _ character -> character.IsHealthy)

    let getCharactersWounded battle =
        getCharacters battle |>
        Map.filter (fun _ character -> character.IsWounded)

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
        { battle with Characters_ = Map.map (fun i c -> if predicate i c then updater c else c) battle.Characters_ }

    let updateCharacters updater battle =
        updateCharactersIf tautology2 updater battle

    let updateCharactersHealthy updater battle =
        updateCharactersIf (fun _ character -> character.IsHealthy) updater battle

    let updateCharactersWounded updater battle =
        updateCharactersIf (fun _ character -> character.IsWounded) updater battle

    let updateAlliesIf pred updater battle =
        updateCharactersIf (fun i c -> pred i c && match i with AllyIndex _ -> true | _ -> false) updater battle

    let updateAllies updater battle =
        updateAlliesIf tautology2 updater battle

    let updateEnemiesIf pred updater battle =
        updateCharactersIf (fun i c -> pred i c && match i with EnemyIndex _ -> true | _ -> false) updater battle

    let updateEnemies updater battle =
        updateEnemiesIf tautology2 updater battle

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

    let counterAttack sourceIndex targetIndex battle =
        let attackCommand = ActionCommand.make Attack targetIndex (Some sourceIndex)
        prependActionCommand attackCommand battle

    let tryCounterAttack sourceIndex targetIndex battle =
        if Character.shouldCounter target
        then counterAttack sourceIndex targetIndex battle
        else battle

    let rec private tryRandomizeEnemy attempts enemy (layout : Either<unit, EnemyType option> array array) =
        if attempts < 10000 then
            let (w, h) = (layout.Length, layout.[0].Length)
            let (x, y) = (Gen.random1 w, Gen.random1 h)
            match Data.Value.Characters.TryFind (Enemy enemy) with
            | Some characterData ->
                match Data.Value.Archetypes.TryFind characterData.ArchetypeType with
                | Some archetypeData ->
                    match archetypeData.Stature with
                    | SmallStature | NormalStature | LargeStature ->
                        if x > 0 && x < w - 1 && y > 0 && y < h - 1 then
                            match
                                (layout.[x+0].[y+1],
                                 layout.[x-1].[y+0], layout.[x+0].[y+0], layout.[x+1].[y+0],
                                 layout.[x+0].[y-1]) with
                            |   (Left (),
                                 Left (), Left (), Left (),
                                 Left ()) ->
                                layout.[x+0].[y+1] <- Right None
                                layout.[x-1].[y+0] <- Right None; layout.[x+0].[y+0] <- Right (Some enemy); layout.[x+1].[y+0] <- Right None
                                layout.[x+0].[y-1] <- Right None
                            | _ -> tryRandomizeEnemy (inc attempts) enemy layout
                        else tryRandomizeEnemy (inc attempts) enemy layout
                    | HugeStature ->
                        if x > 1 && x < w - 2 && y > 1 && y < h - 2 then 
                            match
                                (layout.[x+0].[y+2],
                                 layout.[x-1].[y+1], layout.[x+0].[y+1], layout.[x+1].[y+1],
                                 layout.[x-2].[y+0], layout.[x-1].[y+0], layout.[x+0].[y+0], layout.[x+1].[y+0], layout.[x+2].[y+0],
                                 layout.[x-1].[y-1], layout.[x+0].[y-1], layout.[x+1].[y-1],
                                 layout.[x+0].[y-2]) with
                            |   (Left (),
                                 Left (), Left (), Left (),
                                 Left (), Left (), Left (), Left (), Left (),
                                 Left (), Left (), Left (),
                                 Left ()) ->
                                layout.[x+0].[y+2] <- Right None
                                layout.[x-1].[y+1] <- Right None; layout.[x+0].[y+1] <- Right None; layout.[x+1].[y+1] <- Right None
                                layout.[x-2].[y+0] <- Right None; layout.[x-1].[y+0] <- Right None; layout.[x+0].[y+0] <- Right (Some enemy); layout.[x+1].[y+0] <- Right None; layout.[x+2].[y+0] <- Right None
                                layout.[x-1].[y-1] <- Right None; layout.[x+0].[y-1] <- Right None; layout.[x+1].[y-1] <- Right None
                                layout.[x+0].[y-2] <- Right None
                            | _ -> tryRandomizeEnemy (inc attempts) enemy layout
                        else tryRandomizeEnemy (inc attempts) enemy layout
                | None -> ()
            | None -> ()
        else Log.debug ("No enemy fit found for '" + scstring enemy + "' in layout.")

    let private randomizeEnemyLayout w h enemies =
        let layout = Array.init w (fun _ -> Array.init h (fun _ -> Left ()))
        layout.[0].[0] <- Left () // don't put enemies in the corners
        layout.[w-1].[0] <- Left ()
        layout.[0].[h-1] <- Left ()
        layout.[w-1].[h-1] <- Left ()
        List.iter (fun enemy -> tryRandomizeEnemy 0 enemy layout) enemies
        layout

    let private randomizeEnemies offsetCharacters enemies =
        let (w, h) = (10, 8)
        let origin = v2 -288.0f -240.0f
        let tile = v2 48.0f 48.0f
        let layout = randomizeEnemyLayout w h enemies
        let enemies =
            let mutable index = -1
            layout |>
            Array.mapi (fun x arr ->
                Array.mapi (fun y enemyOpt ->
                    match enemyOpt with
                    | Left () -> None
                    | Right None -> None
                    | Right (Some enemy) ->
                        index <- inc index
                        let position = v2 (origin.X + single x * tile.X) (origin.Y + single y * tile.Y)
                        Character.tryMakeEnemy index offsetCharacters { EnemyType = enemy; EnemyPosition = position })
                    arr) |>
            Array.concat |>
            Array.definitize |>
            Array.toList
        enemies

    let makeFromParty offsetCharacters inventory (prizePool : PrizePool) (party : Party) battleData time =
        let enemies = randomizeEnemies offsetCharacters battleData.BattleEnemies
        let characters = party @ enemies |> Map.ofListBy (fun (character : Character) -> (character.CharacterIndex, character))
        let prizePool = { prizePool with Gold = List.fold (fun gold (enemy : Character) -> gold + enemy.GoldPrize) prizePool.Gold enemies }
        let prizePool = { prizePool with Exp = List.fold (fun exp (enemy : Character) -> exp + enemy.ExpPrize) prizePool.Exp enemies }
        let tileMap = battleData.BattleTileMap
        let battle =
            { BattleState_ = BattleReady time
              Characters_ = characters
              Inventory_ = inventory
              PrizePool_ = prizePool
              TileMap_ = tileMap
              BattleSongOpt_ = battleData.BattleSongOpt
              CurrentCommandOpt_ = None
              ActionCommands_ = Queue.empty
              DialogOpt_ = None }
        battle

    let makeFromTeam inventory prizePool (team : Map<int, Teammate>) battleData time =
        let party = team |> Map.toList |> List.tryTake 3
        let offsetCharacters = List.hasAtMost 1 party
        let allyPositions =
            if List.length party < 3
            then List.take 2 battleData.BattleAllyPositions
            else List.take 1 battleData.BattleAllyPositions @ List.skip 2 battleData.BattleAllyPositions
        let party =
            List.mapi
                (fun index (teamIndex, teammate) ->
                    match Map.tryFind teammate.CharacterType Data.Value.Characters with
                    | Some characterData ->
                        // TODO: bounds checking
                        let size = Constants.Gameplay.CharacterSize
                        let position = if offsetCharacters then allyPositions.[teamIndex] + Constants.Battle.CharacterOffset else allyPositions.[teamIndex]
                        let bounds = v4Bounds position size
                        let characterIndex = AllyIndex teamIndex
                        let characterType = characterData.CharacterType
                        let characterState = CharacterState.make characterData teammate.HitPoints teammate.TechPoints teammate.ExpPoints teammate.WeaponOpt teammate.ArmorOpt teammate.Accessories
                        let animationSheet = characterData.AnimationSheet
                        let direction = Direction.ofVector2 -bounds.Bottom
                        let actionTime = 1000 - 500 * index // TODO: P1: put this in Constants.
                        let character = Character.make bounds characterIndex characterType characterState animationSheet direction actionTime
                        character
                    | None -> failwith ("Could not find CharacterData for '" + scstring teammate.CharacterType + "'."))
                party
        let battle = makeFromParty offsetCharacters inventory prizePool party battleData time
        battle

    let empty =
        { BattleState_ = BattleReady 0L
          Characters_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          PrizePool_ = { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }
          TileMap_ = Assets.Field.DebugBattleTileMap
          BattleSongOpt_ = None
          CurrentCommandOpt_ = None
          ActionCommands_ = Queue.empty
          DialogOpt_ = None }

    let debug =
        match Map.tryFind DebugBattle Data.Value.Battles with
        | Some battle ->
            let level = 9
            let prizePool = { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }
            let team =
                Map.singleton 0 (Teammate.makeAtLevel level 0 Jinn) |>
                Map.add 1 (Teammate.makeAtLevel level 1 Riain) |>
                Map.add 2 (Teammate.makeAtLevel level 2 Peric)
            makeFromTeam Inventory.initial prizePool team battle 0L
        | None -> empty

type Battle = Battle.Battle