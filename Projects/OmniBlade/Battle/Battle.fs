// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open OmniBlade
open OmniBlade.BattleInteractionSystem

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

type ActionCommand =
    { Action : ActionType
      Source : CharacterIndex
      TargetOpt : CharacterIndex option
      ObserverOpt : CharacterIndex option }

    static member make action source targetOpt observerOpt =
        { Action = action
          Source = source
          TargetOpt = targetOpt
          ObserverOpt = observerOpt }

type CurrentCommand =
    { StartTime : int64
      ActionCommand : ActionCommand }

    static member make startTime actionCommand =
        { StartTime = startTime; ActionCommand = actionCommand }

[<RequireQualifiedAccess>]
module Battle =

    type [<ReferenceEquality; SymbolicExpansion>] Battle =
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
        Map.filter (fun _ character -> character.Healthy)

    let getCharactersWounded battle =
        getCharacters battle |>
        Map.filter (fun _ character -> character.Wounded)

    let getCharactersHudded battle =
        getCharactersIf (fun _ (character : Character) ->
            character.Ally ||
            (character.Enemy && not character.Wounding && not character.Materializing))
            battle

    let getAllies battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Map.ofSeq

    let getAlliesHealthy battle =
        getAllies battle |>
        Map.filter (fun _ character -> character.Healthy)

    let getAlliesWounded battle =
        getAllies battle |>
        Map.filter (fun _ character -> character.Wounded)

    let getEnemies battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Map.ofSeq

    let getEnemiesHealthy battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, enemy) -> not enemy.Wounding | _ -> false) |> Map.ofSeq

    let getEnemiesWounded battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, enemy) -> enemy.Wounding | _ -> false) |> Map.ofSeq

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

    let nextEnemyIndex battle =
        let mutable lastIndex = 0
        for entry in getEnemies battle do
            if entry.Key.Subindex > lastIndex then
                lastIndex <- entry.Key.Subindex
        let enemySubindex = inc lastIndex
        EnemyIndex enemySubindex

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
        updateCharactersIf (fun _ character -> character.Healthy) updater battle

    let updateCharactersWounded updater battle =
        updateCharactersIf (fun _ character -> character.Wounded) updater battle

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

    let getCharacterHealthy characterIndex battle =
        (getCharacter characterIndex battle).Healthy

    let getCharacterWounded characterIndex battle =
        (getCharacter characterIndex battle).Wounded

    let getCharacterPerimeterOriginal characterIndex battle =
        (getCharacter characterIndex battle).PerimeterOriginal

    let getCharacterPerimeter characterIndex battle =
        (getCharacter characterIndex battle).Perimeter

    let getCharacterAnimationFinished time characterIndex battle =
        getCharacterBy (Character.getAnimationFinished time) characterIndex battle

    let getCharacterArchetypeType characterIndex battle =
        (getCharacter characterIndex battle).ArchetypeType

    let shouldCounter sourceIndex targetIndex battle =
        if CharacterIndex.unfriendly sourceIndex targetIndex
        then getCharacterBy Character.shouldCounter targetIndex battle
        else false

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

    let updateCharacterInputState updater characterIndex battle =
        updateCharacter (Character.updateCharacterInputState updater) characterIndex battle

    let updateCharacterActionTime updater characterIndex battle =
        updateCharacter (Character.updateActionTime updater) characterIndex battle

    let updateCharacterTechChargeOpt updater characterIndex battle =
        updateCharacter (Character.updateTechChargeOpt updater) characterIndex battle

    let updateCharacterAutoBattleOpt updater characterIndex battle =
        updateCharacter (Character.updateAutoBattleOpt updater) characterIndex battle

    let updateCharacterBottom updater characterIndex battle =
        updateCharacter (Character.updateBottom updater) characterIndex battle

    let updateCharacterHitPoints cancelled affectsWounded hitPointsChange characterIndex battle =
        let alliesHealthy = getAlliesHealthy battle
        let character = getCharacter characterIndex battle
        let character = Character.updateHitPoints (fun hitPoints -> (cancelled, hitPoints + hitPointsChange)) affectsWounded alliesHealthy character
        updateCharacter (constant character) characterIndex battle

    let updateCharacterTechPoints techPointsChange characterIndex battle =
        updateCharacter (Character.updateTechPoints ((+) techPointsChange)) characterIndex battle

    let applyCharacterStatuses added removed characterIndex battle =
        updateCharacter (Character.applyStatusChanges added removed) characterIndex battle

    let defendCharacter characterIndex battle =
        updateCharacter Character.defend characterIndex battle

    let undefendCharacter characterIndex battle =
        updateCharacter Character.undefend characterIndex battle

    let faceCharacter direction characterIndex battle =
        updateCharacter (Character.face direction) characterIndex battle

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
                if character.Ally
                then Character.updateCharacterInputState (constant NoInput) character
                else character
            let character = Character.animate time WoundAnimation character
            character)
            characterIndex
            battle

    let animateCharactersReady time battle =
        updateCharactersHealthy (Character.animate time ReadyAnimation) battle

    let animateCharactersCelebrate time outcome battle =
        if outcome
        then updateAlliesIf (fun _ ally -> ally.Healthy) (Character.animate time CelebrateAnimation) battle
        else updateEnemiesIf (fun _ enemy -> enemy.Healthy) (Character.animate time CelebrateAnimation) battle

    let animateCharactersPoised time battle =
        updateCharactersHealthy (fun character ->
            let poiseType = Character.getPoiseType character
            let character = Character.animate time (PoiseAnimation poiseType) character
            character)
            battle

    let updateBattleState updater battle =
        { battle with BattleState_ = updater battle.BattleState }

    let updateInventory updater battle =
        { battle with Inventory_ = updater battle.Inventory_ }

    let updateCurrentCommandOpt updater battle =
        { battle with CurrentCommandOpt_ = updater battle.CurrentCommandOpt_ }

    let updateActionCommands updater battle =
        { battle with ActionCommands_ = updater battle.ActionCommands_ }

    let updateDialogOpt updater field =
        { field with DialogOpt_ = updater field.DialogOpt_ }

    let halveCharacterActionTime characterIndex battle =
        updateCharacterActionTime (fun at -> min (at * 0.5f) (Constants.Battle.ActionTime * 0.5f)) characterIndex battle

    let resetCharacterActionTime characterIndex battle =
        updateCharacterActionTime (constant 0.0f) characterIndex battle

    let resetCharacterInput (characterIndex : CharacterIndex) battle =
        let battle =
            if characterIndex.Ally
            then updateCharacterInputState (constant NoInput) characterIndex battle
            else updateCharacterAutoBattleOpt (constant None) characterIndex battle
        battle

    let resetCharacterTechCharge characterIndex battle =
        updateCharacter Character.resetTechCharge characterIndex battle

    let resetCharacterConjureCharge characterIndex battle =
        updateCharacter Character.resetConjureCharge characterIndex battle

    let characterAppendedActionCommand characterIndex battle =
        seq battle.ActionCommands_ |>
        Seq.exists (fun command -> command.Source = characterIndex)

    let appendActionCommand command battle =
        { battle with ActionCommands_ = Queue.conj command battle.ActionCommands_ }

    let prependActionCommand command battle =
        { battle with ActionCommands_ = Queue.rev battle.ActionCommands_ |> Queue.conj command |> Queue.rev }

    let counterAttack sourceIndex targetIndex battle =
        let attackCommand = ActionCommand.make Attack targetIndex (Some sourceIndex) None
        prependActionCommand attackCommand battle

    let advanceCharacterConjureCharge characterIndex battle =
        updateCharacter Character.advanceConjureCharge characterIndex battle

    let advanceCharacterStatusBurndown burndownTime characterIndex battle =
        updateCharacter (Character.burndownStatuses burndownTime) characterIndex battle

    let abortCharacterAction time characterIndex battle =
        let battle = updateCurrentCommandOpt (constant None) battle
        let battle = animationCharacterPoise time characterIndex battle
        let battle = updateCharacterActionTime (constant 0.0f) characterIndex battle
        let battle = resetCharacterInput characterIndex battle
        let battle = advanceCharacterConjureCharge characterIndex battle
        battle

    let finishCharacterAction characterIndex battle =
        let battle = advanceCharacterConjureCharge characterIndex battle
        battle

    let evalAttack effectType sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        Character.getAttackResult effectType source target

    let evalTechUnary targetCount techData sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        (techData.TechCost, Character.evalTechUnary targetCount techData source target)

    let evalTech sourceIndex targetIndex techType battle =
        match Map.tryFind techType Data.Value.Techs with
        | Some techData ->
            let source = getCharacter sourceIndex battle
            let target = getCharacter targetIndex battle
            let characters = getCharacters battle
            Triple.prepend techData.TechCost (Character.evalTech techData source target characters)
        | None -> (0, None, Map.empty)

    let tryChargeCharacter chargeAmount characterIndex battle =
        updateCharacter
            (Character.updateTechChargeOpt
                (function
                 | Some (chargeRate, chargeTime, techType) -> Some (chargeRate, max 0 (min Constants.Battle.ChargeMax (chargeAmount + chargeTime)), techType)
                 | None -> None))
            characterIndex
            battle

    let tryRetargetIfNeeded affectingWounded targetIndexOpt battle =
        match targetIndexOpt with
        | Some targetIndex ->
            if affectingWounded then
                match tryGetCharacterBy (fun (target : Character) -> target.Healthy) targetIndex battle with
                | Some true | None ->
                    match targetIndex with
                    | AllyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getAlliesWounded battle)))
                    | EnemyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getEnemiesWounded battle)))
                | Some false -> targetIndexOpt
            else
                match tryGetCharacterBy (fun (target : Character) -> target.Wounded) targetIndex battle with
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
                    | Consequence _ | ActionType.Wound -> failwithumf ()
                Character.updateCharacterInputState (constant inputState) character
            | None -> character)
            characterIndex
            battle

    let confirmCharacterInput sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        match Character.getActionTypeOpt source with
        | Some actionType ->
            let command = ActionCommand.make actionType sourceIndex (Some targetIndex) None
            appendActionCommand command battle
        | None -> battle

    let populateAllyConjureCharges battle =
        updateAllies (fun ally ->
            if Character.hasConjureTechs ally
            then Character.updateConjureChargeOpt (constant (Some 0)) ally
            else ally)
            battle

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
            match Data.Value.Characters.TryFind (CharacterType.Enemy enemy) with
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

    let private randomizeEnemies allyCount waitSpeed enemies =
        let origin = v2 -288.0f -240.0f // TODO: P1: turn these duplicated vars into global consts.
        let tile = v2 48.0f 48.0f
        let (w, h) = (10, 8)
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
                        Character.tryMakeEnemy allyCount enemyIndex waitSpeed { EnemyType = enemy; EnemyPosition = position })
                    arr) |>
            Array.concat |>
            Array.definitize |>
            Array.toList
        enemies

    let spawnEnemies time (spawn : SpawnType list) battle =
        let origin = v2 -288.0f -240.0f // TODO: P1: turn these duplicated vars into global consts.
        let tile = v2 48.0f 48.0f
        let (w, h) = (10, 8)
        let waitSpeed = battle.BattleSpeed_ = WaitSpeed
        let allyCount = battle |> getAllies |> Map.count
        let battle =
            List.fold (fun battle (spawnType : SpawnType) ->
                let mutable battle = battle
                let mutable spawned = false
                let mutable tries = 0
                while not spawned && tries < 100 do
                    let (i, j) = (Gen.random1 w, Gen.random1 h)
                    let position = v3 (origin.X + single i * tile.X) (origin.Y + single j * tile.Y) 0.0f
                    let positions = battle |> getEnemies |> Map.toValueArray |> Array.map (fun (enemy : Character) -> enemy.PerimeterOriginal.BottomLeft)
                    let notOnSides = i <> 0 && i <> w - 1
                    let notOverlapping = Array.notExists (fun position' -> Vector3.Distance (position, position') < tile.X * 2.0f) positions
                    if notOnSides && notOverlapping then
                        let enemyIndex = nextEnemyIndex battle                        
                        match Character.tryMakeEnemy allyCount enemyIndex.Subindex waitSpeed { EnemyType = spawnType.EnemyType; EnemyPosition = position } with
                        | Some enemy ->
                            let enemy =
                                match spawnType.SpawnEffectType with
                                | Materialize -> Character.materialize enemy
                                | Unearth -> Character.animate time UnearthAnimation enemy
                            battle <- addCharacter enemyIndex enemy battle
                            spawned <- true
                        | None -> ()
                    tries <- inc tries
                battle)
                battle
                spawn
        battle

    let rec evalFightAffectType affectType (source : Character) (target : Character) (observer : Character) battle =
        match affectType with
        | BattleAffectType.Physical -> true
        | HpLessThanOrEqual ceiling -> single target.HitPointsMax / single target.HitPoints <= ceiling
        | HpGreaterThanOrEqual floor -> single target.HitPointsMax / single target.HitPoints >= floor
        | TpLessThanOrEqual ceiling -> single target.TechPointsMax / single target.TechPoints <= ceiling
        | TpGreaterThanOrEqual floor -> single target.TechPointsMax / single target.TechPoints >= floor
        | OneEnemyLeft ->
            let enemies = if observer.Ally then getEnemies battle else getAllies battle
            enemies.Count = 1
        | Wound -> target.HitPoints <= 0
        | Random chance -> Gen.randomf < chance
        | Any affectTypes -> List.exists (fun affectType -> evalFightAffectType affectType source target observer battle) affectTypes
        | All affectTypes -> List.forall (fun affectType -> evalFightAffectType affectType source target observer battle) affectTypes
        | _ -> false

    let rec evalFightTargetType targetType (source : Character) (target : Character) (observer : Character) battle =
        match targetType with
        | Self ->
            observer = target
        | Other ->
            observer <> target
        | Ally ->
            let allies = if observer.Ally then getAllies battle else getEnemies battle
            Map.containsKey target.CharacterIndex allies
        | OtherAlly ->
            let allies = if observer.Ally then getAllies battle else getEnemies battle
            observer <> target && Map.containsKey target.CharacterIndex allies
        | OtherEnemy | Enemy ->
            let enemies = if observer.Ally then getEnemies battle else getAllies battle
            Map.containsKey target.CharacterIndex enemies
        | BattleTargetType.Any targetTypes -> List.exists (fun targetType -> evalFightTargetType targetType source target observer battle) targetTypes
        | BattleTargetType.All targetTypes -> List.forall (fun targetType -> evalFightTargetType targetType source target observer battle) targetTypes

    let evalFightInteractions4 (source : Character) (target : Character) (observer : Character) battle =
        List.fold (fun consequences interaction ->
            let condition = interaction.BattleCondition
            let consequences' = interaction.BattleConsequences
            let satisfied =
                match condition with
                | WhenOnlySurvivor ->
                    let allies = if target.Ally then getAllies battle else getEnemies battle
                    observer = target && allies.Count < 2
                | WhenOnlyTypeSurviving ->
                    let allies = if target.Ally then getAllies battle else getEnemies battle
                    observer = target &&
                    Seq.filter (fun (ally : Character) -> ally.ArchetypeType = observer.ArchetypeType && ally.HitPoints > 0) allies.Values |>
                    Seq.length = 1
                | WhenTargetAffected (affectType, targetType) ->
                    evalFightAffectType affectType source target observer battle &&
                    evalFightTargetType targetType source target observer battle
            if satisfied then consequences' @ consequences else consequences)
            [] target.Interactions

    let evalFightInteractions (source : Character) (target : Character) battle =
        let characters = getCharacters battle
        Seq.fold (fun consequences observer ->
            let consequences' = evalFightInteractions4 source target observer battle
            (source, target, observer, consequences') :: consequences)
            [] characters.Values

    let evalConsequence (source : Character) (target : Character) (observer : Character) consequence battle =
        appendActionCommand (ActionCommand.make (Consequence consequence) source.CharacterIndex (Some target.CharacterIndex) (Some observer.CharacterIndex)) battle

    let evalConsequences consequenceses battle =
        List.fold (fun battle (source, target, observer, consequences) ->
            List.fold (fun battle consequence ->
                evalConsequence source target observer consequence battle)
                battle consequences)
            battle consequenceses

    let makeFromParty inventory (prizePool : PrizePool) (party : Party) battleSpeed battleData world =
        let enemies = randomizeEnemies party.Length (battleSpeed = WaitSpeed) battleData.BattleEnemies
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
              BattleSpeed_ = PacedSpeed
              CurrentCommandOpt_ = None
              ActionCommands_ = Queue.empty
              DialogOpt_ = None }
        | None -> failwith "Expected data for DebugBattle to be available."

type Battle = Battle.Battle