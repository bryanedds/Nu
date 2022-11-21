// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type BattleSpeed =
    | SwiftSpeed
    | PacedSpeed
    | WaitSpeed

type BattleState =
    | BattleReady of int64
    | BattleRunning
    | BattleResult of int64 * bool
    | BattleQuitting of int64 * bool * Advent Set
    | BattleQuit

type [<NoComparison>] ActionCommand =
    { Action : ActionType
      Source : CharacterIndex
      TargetOpt : CharacterIndex option }

    static member make action source targetOpt =
        { Action = action
          Source = source
          TargetOpt = targetOpt }

type [<NoComparison>] CurrentCommand =
    { StartTime : int64
      ActionCommand : ActionCommand }

    static member make startTime actionCommand =
        { StartTime = startTime; ActionCommand = actionCommand }

[<RequireQualifiedAccess>]
module Battle =

    type [<NoComparison>] Battle =
        private
            { BattleState_ : BattleState
              Characters_ : Map<CharacterIndex, Character>
              Inventory_ : Inventory
              PrizePool_ : PrizePool
              TileMap_ : TileMap AssetTag
              TileIndexOffset_ : int
              TileIndexOffsetRange_ : int * int
              BattleSongOpt_ : Song AssetTag option
              BattleSpeed_ : BattleSpeed
              CurrentCommandOpt_ : CurrentCommand option
              ActionCommands_ : ActionCommand Queue
              DialogOpt_ : Dialog option }

        (* Local Properties *)
        member this.Running = match this.BattleState with BattleRunning _ -> true | _ -> false
        member this.NotRunning = not this.Running
        member this.BattleState = this.BattleState_
        member this.Characters = this.Characters_
        member this.Inventory = this.Inventory_
        member this.PrizePool = this.PrizePool_
        member this.TileMap = this.TileMap_
        member this.TileIndexOffset = this.TileIndexOffset_
        member this.TileIndexOffsetRange = this.TileIndexOffsetRange_
        member this.BattleSongOpt = this.BattleSongOpt_
        member this.BattleSpeed = this.BattleSpeed_
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

    let getAlliesHealthy battle =
        getAllies battle |>
        Map.filter (fun _ character -> character.IsHealthy)

    let getAlliesWounded battle =
        getAllies battle |>
        Map.filter (fun _ character -> character.IsWounded)

    let getEnemies battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Map.ofSeq

    let getEnemiesHealthy battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, enemy) -> not enemy.IsWounding | _ -> false) |> Map.ofSeq

    let getEnemiesWounded battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, enemy) -> enemy.IsWounding | _ -> false) |> Map.ofSeq

    let getTargets aimType battle =
        match aimType with
        | EnemyAim _ ->
            getEnemiesHealthy battle
        | AllyAim healthy ->
            if healthy
            then getAlliesHealthy battle
            else getAlliesWounded battle
        | AnyAim healthy ->
            let allies =
                if healthy
                then getAlliesHealthy battle
                else getAlliesWounded battle
            let enemies =
                if healthy
                then getEnemiesHealthy battle
                else getEnemiesWounded battle
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

    let tryGetCharacterBy by characterIndex battle =
        match Map.tryFind characterIndex battle.Characters_ with
        | Some character -> Some (by character)
        | None -> None

    let getCharacter characterIndex battle =
        tryGetCharacter characterIndex battle |> Option.get

    let getCharacterBy by characterIndex battle =
        tryGetCharacter characterIndex battle |> Option.get |> by

    let isCharacterHealthy characterIndex battle =
        (getCharacter characterIndex battle).IsHealthy

    let isCharacterWounded characterIndex battle =
        (getCharacter characterIndex battle).IsWounded

    let getCharacterPerimeterOriginal characterIndex battle =
        (getCharacter characterIndex battle).PerimeterOriginal

    let getCharacterPerimeter characterIndex battle =
        (getCharacter characterIndex battle).Perimeter

    let getCharacterAnimationFinished time characterIndex battle =
        getCharacterBy (Character.getAnimationFinished time) characterIndex battle

    let getCharacterArchetypeType characterIndex battle =
        (getCharacter characterIndex battle).ArchetypeType

    let private tryUpdateCharacter updater characterIndex battle =
        match tryGetCharacter characterIndex battle with
        | Some character ->
            let character = updater character
            { battle with Characters_ = Map.add characterIndex character battle.Characters_ }
        | None -> battle

    let private updateCharacter updater characterIndex battle =
        let character = getCharacter characterIndex battle
        let character = updater character
        { battle with Characters_ = Map.add characterIndex character battle.Characters_ }

    let updateCharacterActionTime updater characterIndex battle =
        updateCharacter (Character.updateActionTime updater) characterIndex battle

    let updateCharacterChargeTechOpt updater characterIndex battle =
        updateCharacter (Character.updateChargeTechOpt updater) characterIndex battle

    let updateCharacterAutoBattleOpt updater characterIndex battle =
        updateCharacter (Character.updateAutoBattleOpt updater) characterIndex battle

    let updateCharacterInputState updater characterIndex battle =
        updateCharacter (Character.updateInputState updater) characterIndex battle

    let updateCharacterBottom updater characterIndex battle =
        updateCharacter (Character.updateBottom updater) characterIndex battle

    let updateCharacterHitPoints cancelled affectsWounded hitPointsChange characterIndex battle =
        let alliesHealthy = getAlliesHealthy battle
        let character = getCharacter characterIndex battle
        let character = Character.updateHitPoints (fun hitPoints -> (cancelled, hitPoints + hitPointsChange)) affectsWounded alliesHealthy character
        updateCharacter (constant character) characterIndex battle

    let updateCharacterTechPoints techPointsChange characterIndex battle =
        updateCharacter (Character.updateTechPoints ((+) techPointsChange)) characterIndex battle

    let burndownCharacterStatuses burndownTime characterIndex battle =
        updateCharacter (Character.burndownStatuses burndownTime) characterIndex battle

    let applyCharacterStatuses added removed characterIndex battle =
        updateCharacter (Character.applyStatusChanges added removed) characterIndex battle

    let defendCharacter characterIndex battle =
        updateCharacter Character.defend characterIndex battle

    let undefendCharacter characterIndex battle =
        updateCharacter Character.undefend characterIndex battle

    let animateCharacter time animation characterIndex battle =
        updateCharacter (Character.animate time animation) characterIndex battle

    let animationCharacterPoise time characterIndex battle =
        updateCharacter (fun character ->
            let poiseType = Character.getPoiseType character
            let character = Character.animate time (PoiseAnimation poiseType) character
            character)
            characterIndex
            battle

    let animateCharacterWound time characterIndex battle =
        updateCharacter (fun character ->
            let character =
                if character.IsAlly
                then Character.updateInputState (constant NoInput) character
                else character
            let character = Character.animate time WoundAnimation character
            character)
            characterIndex
            battle

    let animateCharactersReady time battle =
        updateCharactersHealthy (Character.animate time ReadyAnimation) battle

    let animateCharactersCelebrate time outcome battle =
        if outcome
        then updateAlliesIf (fun _ ally -> ally.IsHealthy) (Character.animate time CelebrateAnimation) battle
        else updateEnemiesIf (fun _ enemy -> enemy.IsHealthy) (Character.animate time CelebrateAnimation) battle

    let animatedCharactersPoised time battle =
        updateCharactersHealthy (fun character ->
            let poiseType = Character.getPoiseType character
            let character = Character.animate time (PoiseAnimation poiseType) character
            character)
            battle

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

    let advanceChargeTech characterIndex battle =
        updateCharacter Character.advanceChargeTech characterIndex battle

    let counterAttack sourceIndex targetIndex battle =
        let attackCommand = ActionCommand.make Attack targetIndex (Some sourceIndex)
        prependActionCommand attackCommand battle

    let shouldCounter sourceIndex targetIndex battle =
        if CharacterIndex.isUnfriendly sourceIndex targetIndex
        then getCharacterBy Character.shouldCounter targetIndex battle
        else false

    let evalAttack effectType sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        Character.getAttackResult effectType source target

    let evalTech targetCount techData sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        (techData.TechCost, Character.evalTech targetCount techData source target)

    let evalTechMove sourceIndex targetIndex techType battle =
        match Map.tryFind techType Data.Value.Techs with
        | Some techData ->
            let source = getCharacter sourceIndex battle
            let target = getCharacter targetIndex battle
            let characters = getCharacters battle
            (techData.TechCost, Character.evalTechMove techData source target characters)
        | None -> (0, Map.empty)

    let tryRetargetIfNeeded affectingWounded targetIndexOpt battle =
        match targetIndexOpt with
        | Some targetIndex ->
            if affectingWounded then
                match tryGetCharacterBy (fun (target : Character) -> target.IsHealthy) targetIndex battle with
                | Some true | None ->
                    match targetIndex with
                    | AllyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getAlliesWounded battle)))
                    | EnemyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getEnemiesWounded battle)))
                | Some false -> targetIndexOpt
            else
                match tryGetCharacterBy (fun (target : Character) -> target.IsWounded) targetIndex battle with
                | Some true | None ->
                    match targetIndex with
                    | AllyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getAlliesHealthy battle)))
                    | EnemyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getEnemiesHealthy battle)))
                | Some false -> targetIndexOpt
        | None -> targetIndexOpt

    let cancelCharacterInput characterIndex battle =
        tryUpdateCharacter (fun character ->
            match Character.getActionTypeOpt character with
            | Some actionType ->
                let inputState =
                    match actionType with
                    | Attack -> RegularMenu
                    | Defend -> RegularMenu
                    | Tech _ -> TechMenu
                    | Consume _ -> ItemMenu
                    | Wound -> failwithumf ()
                Character.updateInputState (constant inputState) character
            | None -> character)
            characterIndex
            battle

    let confirmCharacterInput sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        match Character.getActionTypeOpt source with
        | Some actionType ->
            let command = ActionCommand.make actionType sourceIndex (Some targetIndex)
            appendActionCommand command battle
        | None -> battle

    let autoBattleEnemies battle =
        let alliesHealthy = getAlliesHealthy battle
        let alliesWounded = getAlliesWounded battle
        let enemiesHealthy = getEnemiesHealthy battle
        let enemiesWounded = getEnemiesWounded battle
        updateEnemies (Character.autoBattle alliesHealthy alliesWounded enemiesHealthy enemiesWounded) battle

    let rec private tryRandomizeEnemy attempts index enemy (layout : Either<unit, (int * EnemyType) option> array array) =
        if attempts < 10000 then
            let (w, h) = (layout.Length, layout.[0].Length)
            let (x, y) = (Gen.random1 w, Gen.random1 h)
            match Data.Value.Characters.TryFind (Enemy enemy) with
            | Some characterData ->
                match Data.Value.Archetypes.TryFind characterData.ArchetypeType with
                | Some archetypeData ->
                    match archetypeData.Stature with
                    | SmallStature | NormalStature | LargeStature ->
                        if x > 0 && x < w - 1 && y < h - 1 then
                            match
                                (layout.[x-1].[y+1], layout.[x+0].[y+1], layout.[x+1].[y+1],
                                 layout.[x-1].[y+0], layout.[x+0].[y+0], layout.[x+1].[y+0]) with
                            |   (Left (), Left (), Left (),
                                 Left (), Left (), Left ()) ->
                                layout.[x-1].[y+1] <- Right None; layout.[x+0].[y+1] <- Right None; layout.[x+1].[y+1] <- Right None
                                layout.[x-1].[y+0] <- Right None; layout.[x+0].[y+0] <- Right (Some (index, enemy)); layout.[x+1].[y+0] <- Right None
                            | _ -> tryRandomizeEnemy (inc attempts) index enemy layout
                        else tryRandomizeEnemy (inc attempts) index enemy layout
                    | BossStature ->
                        if x > 1 && x < w - 2 && y > 0 && y < h - 3 then 
                            match
                                (layout.[x-2].[y+3], layout.[x-1].[y+3], layout.[x+0].[y+3], layout.[x+1].[y+3], layout.[x+2].[y+3],
                                 layout.[x-2].[y+2], layout.[x-1].[y+2], layout.[x+0].[y+2], layout.[x+1].[y+2], layout.[x+2].[y+2],
                                 layout.[x-2].[y+1], layout.[x-1].[y+1], layout.[x+0].[y+1], layout.[x+1].[y+1], layout.[x+2].[y+1],
                                 layout.[x-2].[y+0], layout.[x-1].[y+0], layout.[x+0].[y+0], layout.[x+1].[y+0], layout.[x+2].[y+0],
                                 layout.[x-2].[y-1], layout.[x-1].[y-1], layout.[x+0].[y-1], layout.[x+1].[y-1], layout.[x+2].[y-1]) with
                            |   (Left (), Left (), Left (), Left (), Left (),
                                 Left (), Left (), Left (), Left (), Left (),
                                 Left (), Left (), Left (), Left (), Left (),
                                 Left (), Left (), Left (), Left (), Left (),
                                 Left (), Left (), Left (), Left (), Left ()) ->
                                layout.[x-2].[y+3] <- Right None; layout.[x-1].[y+3] <- Right None; layout.[x+0].[y+3] <- Right None; layout.[x+1].[y+3] <- Right None; layout.[x+2].[y+3] <- Right None
                                layout.[x-2].[y+2] <- Right None; layout.[x-1].[y+2] <- Right None; layout.[x+0].[y+2] <- Right None; layout.[x+1].[y+2] <- Right None; layout.[x+2].[y+2] <- Right None
                                layout.[x-2].[y+1] <- Right None; layout.[x-1].[y+1] <- Right None; layout.[x+0].[y+1] <- Right None; layout.[x+1].[y+1] <- Right None; layout.[x+2].[y+1] <- Right None
                                layout.[x-2].[y+0] <- Right None; layout.[x-1].[y+0] <- Right None; layout.[x+0].[y+0] <- Right (Some (index, enemy)); layout.[x+1].[y+0] <- Right None; layout.[x+2].[y+0] <- Right None
                                layout.[x-2].[y-1] <- Right None; layout.[x-1].[y-1] <- Right None; layout.[x+0].[y-1] <- Right None; layout.[x+1].[y-1] <- Right None; layout.[x+2].[y-1] <- Right None
                            | _ -> tryRandomizeEnemy (inc attempts) index enemy layout
                        else tryRandomizeEnemy (inc attempts) index enemy layout
                | None -> ()
            | None -> ()
        else Log.info ("No enemy fit found for '" + scstring enemy + "' in layout.")

    let private randomizeEnemyLayout w h (enemies : EnemyType list) =
        let layout = Array.init w (fun _ -> Array.init h (fun _ -> Left ()))
        layout.[0].[0] <- Left () // no one puts enemy in a corner
        layout.[w-1].[0] <- Left ()
        layout.[0].[h-1] <- Left ()
        layout.[w-1].[h-1] <- Left ()
        List.iteri (fun index enemy -> tryRandomizeEnemy 0 index enemy layout) enemies
        layout

    let private randomizeEnemies allyCount offsetCharacters waitSpeed enemies =
        let (w, h) = (10, 8)
        let origin = v2 -288.0f -240.0f
        let tile = v2 48.0f 48.0f
        let layout = randomizeEnemyLayout w h enemies
        let enemies =
            layout |>
            Array.mapi (fun x arr ->
                Array.mapi (fun y enemyOpt ->
                    match enemyOpt with
                    | Left () -> None
                    | Right None -> None
                    | Right (Some (enemyIndex, enemy)) ->
                        let position = v3 (origin.X + single x * tile.X) (origin.Y + single y * tile.Y) 0.0f
                        Character.tryMakeEnemy allyCount enemyIndex offsetCharacters waitSpeed { EnemyType = enemy; EnemyPosition = position })
                    arr) |>
            Array.concat |>
            Array.definitize |>
            Array.toList
        enemies

    let makeFromParty offsetCharacters inventory (prizePool : PrizePool) (party : Party) battleSpeed battleData world =
        let enemies = randomizeEnemies party.Length offsetCharacters (battleSpeed = WaitSpeed) battleData.BattleEnemies
        let characters = party @ enemies |> Map.ofListBy (fun (character : Character) -> (character.CharacterIndex, character))
        let prizePool = { prizePool with Gold = List.fold (fun gold (enemy : Character) -> gold + enemy.GoldPrize) prizePool.Gold enemies }
        let prizePool = { prizePool with Exp = List.fold (fun exp (enemy : Character) -> exp + enemy.ExpPrize) prizePool.Exp enemies }
        let prizePool = { prizePool with Items = List.fold (fun items (enemy : Character) -> match enemy.ItemPrizeOpt with Some item -> item :: items | None -> items) prizePool.Items enemies }
        let tileMap = battleData.BattleTileMap
        let tileIndexOffset = battleData.BattleTileIndexOffset
        let tileIndexOffsetRange = battleData.BattleTileIndexOffsetRange
        let battle =
            { BattleState_ = BattleReady (World.getUpdateTime world)
              Characters_ = characters
              Inventory_ = inventory
              PrizePool_ = prizePool
              TileMap_ = tileMap
              TileIndexOffset_ = tileIndexOffset
              TileIndexOffsetRange_ = tileIndexOffsetRange
              BattleSongOpt_ = battleData.BattleSongOpt
              BattleSpeed_ = battleSpeed
              CurrentCommandOpt_ = None
              ActionCommands_ = Queue.empty
              DialogOpt_ = None }
        battle

    let makeFromTeam inventory prizePool (team : Map<int, Teammate>) battleSpeed battleData world =
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
                        let celSize = Constants.Gameplay.CharacterCelSize
                        let position = if offsetCharacters then allyPositions.[teamIndex] + Constants.Battle.CharacterOffset else allyPositions.[teamIndex]
                        let bounds = box3 position size
                        let characterIndex = AllyIndex teamIndex
                        let characterType = characterData.CharacterType
                        let characterState = CharacterState.make characterData teammate.HitPoints teammate.TechPoints teammate.ExpPoints teammate.WeaponOpt teammate.ArmorOpt teammate.Accessories
                        let animationSheet = characterData.AnimationSheet
                        let direction = Direction.ofVector3 -bounds.Bottom
                        let actionTime = 1000.0f - Constants.Battle.AllyActionTimeSpacing * single index
                        let character = Character.make bounds characterIndex characterType characterState animationSheet celSize direction None actionTime
                        character
                    | None -> failwith ("Could not find CharacterData for '" + scstring teammate.CharacterType + "'."))
                party
        let battle = makeFromParty offsetCharacters inventory prizePool party battleSpeed battleData world
        battle

    let empty =
        match Map.tryFind EmptyBattle Data.Value.Battles with
        | Some battle ->
            { BattleState_ = BattleQuit
              Characters_ = Map.empty
              Inventory_ = Inventory.empty
              PrizePool_ = PrizePool.empty
              TileMap_ = battle.BattleTileMap
              TileIndexOffset_ = 0
              TileIndexOffsetRange_ = (0, 0)
              BattleSongOpt_ = None
              BattleSpeed_ = SwiftSpeed
              CurrentCommandOpt_ = None
              ActionCommands_ = Queue.empty
              DialogOpt_ = None }
        | None -> failwith "Expected data for DebugBattle to be available."

    let debug world =
        match Map.tryFind DebugBattle Data.Value.Battles with
        | Some battle ->
            let level = 50
            let team =
                Map.singleton 0 (Teammate.make level 0 Jinn) |>
                Map.add 1 (Teammate.make level 1 Peric) |>
                Map.add 2 (Teammate.make level 2 Mael)
            makeFromTeam Inventory.initial PrizePool.empty team SwiftSpeed battle world
        | None -> empty

type Battle = Battle.Battle