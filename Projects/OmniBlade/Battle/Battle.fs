// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open BattleInteractionSystem

type BattleMessage =
    | Update
    | UpdateRideTokens of Map<string, Effects.Slice>
    | TimeUpdate
    | InteractDialog
    | RegularItemSelect of CharacterIndex * string
    | RegularItemCancel of CharacterIndex
    | ConsumableItemSelect of CharacterIndex * string
    | ConsumableItemCancel of CharacterIndex
    | TechItemSelect of CharacterIndex * string
    | TechItemCancel of CharacterIndex
    | ReticlesSelect of CharacterIndex * CharacterIndex
    | ReticlesCancel of CharacterIndex
    | Nop
    interface Message

type BattleCommand =
    | UpdateEye
    | PlaySound of int64 * single * Sound AssetTag
    | PlaySong of GameTime * GameTime * GameTime * single * Song AssetTag
    | FadeOutSong of GameTime
    | DisplayHop of Vector3 * Vector3
    | DisplayCircle of Vector3 * single
    | DisplayHitPointsChange of CharacterIndex * int
    | DisplayCancel of CharacterIndex
    | DisplayCut of int64 * bool * CharacterIndex
    | DisplayCritical of int64 * CharacterIndex
    | DisplayHeavyCritical of int64 * CharacterIndex
    | DisplayPoisonCut of int64 * CharacterIndex
    | DisplayPowerCut of int64 * CharacterIndex
    | DisplayDispelCut of int64 * CharacterIndex
    | DisplayDoubleCut of int64 * CharacterIndex
    | DisplaySlashSpike of int64 * Vector3 * CharacterIndex
    | DisplaySlashTwister of int64 * Vector3 * CharacterIndex
    | DisplayCycloneBlur of int64 * CharacterIndex * single
    | DisplayBuff of int64 * StatusType * CharacterIndex
    | DisplayDebuff of int64 * StatusType * CharacterIndex
    | DisplayImpactSplash of int64 * CharacterIndex
    | DisplayArcaneCast of int64 * CharacterIndex
    | DisplayHolyCast of int64 * CharacterIndex
    | DisplayDimensionalCast of int64 * CharacterIndex
    | DisplayGenericCast of int64 * CharacterIndex
    | DisplayFire of int64 * CharacterIndex * CharacterIndex
    | DisplayFlame of int64 * CharacterIndex * CharacterIndex
    | DisplayIce of int64 * CharacterIndex
    | DisplaySnowball of int64 * CharacterIndex
    | DisplayBolt of int64 * CharacterIndex
    | DisplayCure of int64 * CharacterIndex
    | DisplayProtect of int64 * CharacterIndex
    | DisplayPurify of int64 * CharacterIndex
    | DisplaySilk of int64 * CharacterIndex
    | DisplayInferno of int64
    | DisplayScatterBolt of int64
    interface Command

type Positioning =
    | Position of Vector3
    | Center of Vector3
    | Bottom of Vector3

type Layering =
    | Under
    | Over

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
      SourceIndex : CharacterIndex
      TargetIndexOpt : CharacterIndex option
      ObserverIndexOpt : CharacterIndex option }

    static member make action sourceIndex targetIndexOpt observerIndexOpt =
        { Action = action
          SourceIndex = sourceIndex
          TargetIndexOpt = targetIndexOpt
          ObserverIndexOpt = observerIndexOpt }

type CurrentCommand =
    { StartTime : int64
      ActionCommand : ActionCommand }

    static member make startTime actionCommand =
        { StartTime = startTime; ActionCommand = actionCommand }

[<RequireQualifiedAccess>]
module Battle =

    type [<ReferenceEquality; SymbolicExpansion>] Battle =
        private
            { BattleTime_ : int64
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
              MessageOpt_ : (int64 * int64 * Dialog) option
              DialogOpt_ : Dialog option
              BattleState_ : BattleState }

        (* Local Properties *)
        member this.BattleTime = this.BattleTime_
        member this.Running = match this.BattleState with BattleRunning -> true | _ -> false
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
        member this.MessageOpt = this.MessageOpt_
        member this.DialogOpt = this.DialogOpt_
        member this.BattleState = this.BattleState_

    (* Low-Level Operations *)

    let private mapBattleState updater battle =
        { battle with BattleState_ = updater battle.BattleState_ }

    let private mapInventory updater battle =
        { battle with Inventory_ = updater battle.Inventory_ }

    let private sortActionCommands battle =
        let actionCommands = Array.ofSeq battle.ActionCommands_
        let actionCommandsSorted = Array.sortStableBy (fun command -> match command.Action with Wound -> 0 | Consequence _ -> 1 | _ -> 2) actionCommands
        { battle with ActionCommands_ = Queue.ofSeq actionCommandsSorted }

    let private mapMessageOpt updater field =
        { field with MessageOpt_ = updater field.MessageOpt_ }

    let mapDialogOpt updater field =
        { field with DialogOpt_ = updater field.DialogOpt_ }

    let mapCurrentCommandOpt updater battle =
        { battle with CurrentCommandOpt_ = updater battle.CurrentCommandOpt_ }

    let mapActionCommands updater battle =
        { battle with ActionCommands_ = updater battle.ActionCommands_ }

    let appendActionCommand command battle =
        { battle with ActionCommands_ = Queue.conj command battle.ActionCommands_ }

    let prependActionCommand command battle =
        { battle with ActionCommands_ = Queue.rev battle.ActionCommands_ |> Queue.conj command |> Queue.rev }

    (* Multi-Character Operations *)

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
            (character.Enemy && character.Standing && character.MaterializationOpt.IsNone))
            battle

    let getAllies battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Map.ofSeq

    let getAlliesHealthy battle =
        getAllies battle |>
        Map.filter (fun _ character -> character.Healthy)

    let getAlliesWounded battle =
        getAllies battle |>
        Map.filter (fun _ character -> character.Wounded)

    let getJinnInParty battle =
        getAllies battle |>
        Seq.exists (fun entry -> entry.Value.CharacterType = Ally Jinn)

    let getEnemies battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Map.ofSeq

    let getEnemiesHealthy battle =
        getEnemies battle |>
        Map.filter (fun _ character -> character.Healthy)

    let getEnemiesWounded battle =
        getEnemies battle |>
        Map.filter (fun _ character -> character.Wounded)

    let getEnemiesStanding battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, enemy) -> enemy.Standing | _ -> false) |> Map.ofSeq

    let getEnemiesSwooning battle =
        battle.Characters_ |> Map.toSeq |> Seq.filter (function (EnemyIndex _, enemy) -> enemy.Swooning | _ -> false) |> Map.ofSeq

    let getFriendlies ally battle =
        if ally then getAllies battle else getEnemies battle

    let getFriendliesHealthy ally battle =
        if ally then getAlliesHealthy battle else getEnemiesHealthy battle

    let getFriendliesWounded ally battle =
        if ally then getAlliesWounded battle else getEnemiesWounded battle

    let getUnfriendlies ally battle =
        if ally then getEnemies battle else getAllies battle

    let getUnfriendliesHealthy ally battle =
        if ally then getEnemiesHealthy battle else getAlliesHealthy battle

    let getUnfriendliesWounded ally battle =
        if ally then getEnemiesWounded battle else getAlliesWounded battle

    let getTargets aimType battle =
        match aimType with
        | EnemyAim _ ->
            getEnemiesStanding battle
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
                then getEnemiesStanding battle
                else getEnemiesSwooning battle
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

    let mapCharactersIf predicate updater (battle : Battle) =
        { battle with Characters_ = Map.map (fun i c -> if predicate i c then updater c else c) battle.Characters_ }

    let mapCharacters updater battle =
        mapCharactersIf tautology2 updater battle

    let mapCharactersHealthy updater battle =
        mapCharactersIf (fun _ character -> character.Healthy) updater battle

    let mapCharactersWounded updater battle =
        mapCharactersIf (fun _ character -> character.Wounded) updater battle

    let mapAlliesIf pred updater battle =
        mapCharactersIf (fun i c -> pred i c && match i with AllyIndex _ -> true | _ -> false) updater battle

    let mapAllies updater battle =
        mapAlliesIf tautology2 updater battle

    let mapEnemiesIf pred updater battle =
        mapCharactersIf (fun i c -> pred i c && match i with EnemyIndex _ -> true | _ -> false) updater battle

    let mapEnemies updater battle =
        mapEnemiesIf tautology2 updater battle

    let private finalizeMaterializations battle =
        mapCharacters (fun character ->
            match character.MaterializationOpt with
            | Some Materializing -> Character.materialized battle.BattleTime_ character
            | Some _ | None -> character)
            battle

    let private populateAlliesConjureCharges battle =
        mapAllies (fun ally ->
            if Character.hasConjureTechs ally
            then Character.mapConjureChargeOpt (constant (Some 0)) ally
            else ally)
            battle

    let private autoBattleEnemies battle =
        let jinnInParty = getJinnInParty battle
        let alliesHealthy = getAlliesHealthy battle
        let alliesWounded = getAlliesWounded battle
        let enemiesStanding = getEnemiesStanding battle
        let enemiesSwooning = getEnemiesSwooning battle
        mapEnemies (Character.autoBattle jinnInParty alliesHealthy alliesWounded enemiesStanding enemiesSwooning) battle

    (* Individual Character Operations *)

    let addCharacter index character (battle : Battle) =
        { battle with Characters_ = Map.add index character battle.Characters_ }

    let removeCharacter index (battle : Battle) =
        { battle with Characters_ = Map.remove index battle.Characters_ }

    let containsCharacter characterIndex battle =
        Map.containsKey characterIndex battle.Characters_

    let containsCharacterHealthy characterIndex battle =
        match battle.Characters_.TryGetValue characterIndex with
        | (true, character) -> character.Healthy
        | (false, _) -> false

    let containsCharacterWounded characterIndex battle =
        match battle.Characters_.TryGetValue characterIndex with
        | (true, character) -> character.Wounded
        | (false, _) -> false

    let containsCharacterStanding characterIndex battle =
        match battle.Characters_.TryGetValue characterIndex with
        | (true, character) -> character.Standing
        | (false, _) -> false

    let containsCharacterSwooning characterIndex battle =
        match battle.Characters_.TryGetValue characterIndex with
        | (true, character) -> character.Swooning
        | (false, _) -> false

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

    let getCharacterStanding characterIndex battle =
        (getCharacter characterIndex battle).Standing

    let getCharacterSwooning characterIndex battle =
        (getCharacter characterIndex battle).Swooning

    let getCharacterPerimeterOriginal characterIndex battle =
        (getCharacter characterIndex battle).PerimeterOriginal

    let getCharacterPerimeter characterIndex battle =
        (getCharacter characterIndex battle).Perimeter

    let getCharacterActionTime characterIndex battle =
        getCharacterBy (fun character -> character.ActionTime) characterIndex battle

    let getCharacterAnimationFinished characterIndex battle =
        getCharacterBy (Character.getAnimationFinished battle.BattleTime_) characterIndex battle

    let getCharacterArchetypeType characterIndex battle =
        (getCharacter characterIndex battle).ArchetypeType

    let getCharacterVulnerabilities characterIndex battle =
        (getCharacter characterIndex battle).Vulnerabilities

    let getCharacterAppendedActionCommand characterIndex battle =
        seq battle.ActionCommands_ |>
        Seq.exists (fun command -> command.SourceIndex = characterIndex)

    let shouldCharacterCounter sourceIndex targetIndex battle =
        if CharacterIndex.unfriendly sourceIndex targetIndex
        then getCharacterBy Character.shouldCounter sourceIndex battle
        else false

    let private tryWithCharacter updater characterIndex battle =
        match tryGetCharacter characterIndex battle with
        | Some character ->
            let character = updater character
            { battle with Characters_ = Map.add characterIndex character battle.Characters_ }
        | None -> battle

    let private mapCharacter updater characterIndex battle =
        let character = getCharacter characterIndex battle
        let character = updater character
        { battle with Characters_ = Map.add characterIndex character battle.Characters_ }

    let mapCharacterInputState updater characterIndex battle =
        mapCharacter (Character.mapCharacterInputState updater) characterIndex battle

    let mapCharacterActionTime updater characterIndex battle =
        mapCharacter (Character.mapActionTime updater) characterIndex battle

    let mapCharacterTechChargeOpt updater characterIndex battle =
        mapCharacter (Character.mapTechChargeOpt updater) characterIndex battle

    let mapCharacterAutoBattleOpt updater characterIndex battle =
        mapCharacter (Character.mapAutoBattleOpt updater) characterIndex battle

    let mapCharacterAutoTechOpt updater characterIndex battle =
        mapCharacter (fun character ->
            let techTypeOpt = updater character.AutoBattleOpt
            Character.mapAutoBattleOpt (function Some autoBattle -> Some { autoBattle with AutoTechOpt = techTypeOpt; ChargeTech = techTypeOpt.IsSome } | None -> None) character)
            characterIndex
            battle

    let mapCharacterBottom updater characterIndex battle =
        mapCharacter (Character.mapBottom updater) characterIndex battle

    let mapCharacterHitPoints directAction cancelled affectsWounded hitPointsChange characterIndex battle =
        let alliesHealthy = getAlliesHealthy battle
        let character = getCharacter characterIndex battle
        let character = Character.mapHitPoints (fun hitPoints -> (cancelled, hitPoints + hitPointsChange)) affectsWounded alliesHealthy character
        let character = if directAction then Character.mapStatuses (Map.remove Sleep) character else character
        mapCharacter (constant character) characterIndex battle

    let mapCharacterTechPoints techPointsChange characterIndex battle =
        mapCharacter (Character.mapTechPoints ((+) techPointsChange)) characterIndex battle

    let applyCharacterStatuses added removed characterIndex battle =
        mapCharacter (Character.applyStatusChanges added removed) characterIndex battle

    let applyCharacterVulnerabilities added removed characterIndex battle =
        mapCharacter (Character.applyVulnerabilityChanges added removed) characterIndex battle

    let addCharacterInteraction interaction characterIndex battle =
        mapCharacter (Character.addInteraction interaction) characterIndex battle

    let clearCharacterInteractions characterIndex battle =
        mapCharacter Character.clearInteractions characterIndex battle

    let chargeCharacter chargeAmount characterIndex battle =
        mapCharacter
            (Character.mapTechChargeOpt
                (function
                 | Some (chargeRate, chargeAmount', techType) -> Some (chargeRate, max 0 (min Constants.Battle.ChargeMax (chargeAmount + chargeAmount')), techType)
                 | None -> None))
            characterIndex
            battle

    let defendCharacter characterIndex battle =
        mapCharacter Character.defend characterIndex battle

    let undefendCharacter characterIndex battle =
        mapCharacter Character.undefend characterIndex battle

    let materializeCharacter characterIndex battle =
        mapCharacter (Character.materialize battle.BattleTime_) characterIndex battle

    let dematerializeCharacter characterIndex battle =
        mapCharacter (Character.dematerialize battle.BattleTime_) characterIndex battle

    let faceCharacter direction characterIndex battle =
        mapCharacter (Character.face direction) characterIndex battle

    let faceCharacterAtAutoTarget characterIndex battle =
        let character = getCharacter characterIndex battle
        match character.AutoBattleOpt with
        | Some autoBattle ->
            let target = getCharacter autoBattle.AutoTarget battle
            let sourceToTarget = target.Bottom - character.Bottom
            let direction = if sourceToTarget.X >= 0.0f then Rightward else Leftward // only two directions in this game
            let source = Character.face direction character
            mapCharacter (constant source) characterIndex battle
        | None -> battle

    let animateCharacter animation characterIndex battle =
        mapCharacter (Character.animate battle.BattleTime_ animation) characterIndex battle

    let animationCharacterPoise characterIndex battle =
        mapCharacter (fun character ->
            let poiseType = Character.getPoiseType character
            let character = Character.animate battle.BattleTime_ (PoiseAnimation poiseType) character
            character)
            characterIndex
            battle

    let animateCharacterWound characterIndex battle =
        mapCharacter (fun character ->
            let character =
                if character.Ally
                then Character.mapCharacterInputState (constant NoInput) character
                else character
            let character = Character.animate battle.BattleTime_ WoundAnimation character
            character)
            characterIndex
            battle

    let animateCharactersReady battle =
        mapCharactersHealthy (Character.animate battle.BattleTime_ ReadyAnimation) battle

    let animateCharactersCelebrate outcome battle =
        if outcome
        then mapAlliesIf (fun _ ally -> ally.Healthy) (Character.animate battle.BattleTime_ CelebrateAnimation) battle
        else mapEnemiesIf (fun _ enemy -> enemy.Healthy) (Character.animate battle.BattleTime_ CelebrateAnimation) battle

    let animateCharactersPoised battle =
        mapCharactersHealthy (fun character ->
            let poiseType = Character.getPoiseType character
            let character = Character.animate battle.BattleTime_ (PoiseAnimation poiseType) character
            character)
            battle

    let characterCounterAttack sourceIndex targetIndex battle =
        let battle = prependActionCommand (ActionCommand.make Attack sourceIndex (Some targetIndex) None) battle
        let battle = prependActionCommand (ActionCommand.make (ActionType.Message ("Counter!", 45L)) sourceIndex (Some targetIndex) None) battle
        battle

    let halveCharacterActionTime characterIndex battle =
        mapCharacterActionTime (fun at -> min (at * 0.5f) (Constants.Battle.ActionTime * 0.5f)) characterIndex battle

    let resetCharacterActionTime characterIndex battle =
        mapCharacterActionTime (constant 0.0f) characterIndex battle

    let resetCharacterInput (characterIndex : CharacterIndex) battle =
        let battle =
            if characterIndex.Ally
            then mapCharacterInputState (constant NoInput) characterIndex battle
            else mapCharacterAutoBattleOpt (constant None) characterIndex battle
        battle

    let resetCharacterTechCharge characterIndex battle =
        mapCharacter Character.resetTechCharge characterIndex battle

    let resetCharacterConjureCharge characterIndex battle =
        mapCharacter Character.resetConjureCharge characterIndex battle

    let updateCharacterConjureCharge characterIndex battle =
        mapCharacter Character.updateConjureCharge characterIndex battle

    let updateCharacterStatusBurndown burndownTime characterIndex battle =
        mapCharacter (Character.burndownStatuses burndownTime) characterIndex battle

    let abortCharacterInteraction characterIndex battle =
        let battle =
            // HACK: tries to infer that this action is not coming from a battle interaction consequence when AT = 0.
            // There are some cases where this isn't the correct inference, tho.
            if getCharacterActionTime characterIndex battle = 0.0f
            then updateCharacterConjureCharge characterIndex battle
            else battle
        let battle = mapCharacterAutoBattleOpt (constant None) characterIndex battle
        let battle = mapCharacterActionTime (constant 0.0f) characterIndex battle
        let battle = animationCharacterPoise characterIndex battle
        let battle = resetCharacterInput characterIndex battle
        let battle = mapCurrentCommandOpt (constant None) battle
        battle

    let finishCharacterInteraction characterIndex battle =
        let battle =
            // HACK: tries to infer that this action is not coming from a battle interaction consequence when AT = 0.
            // There are some cases where this isn't the correct inference, tho.
            if getCharacterActionTime characterIndex battle = 0.0f
            then updateCharacterConjureCharge characterIndex battle
            else battle
        let battle = faceCharacterAtAutoTarget characterIndex battle
        battle

    let cancelCharacterInput characterIndex battle =
        tryWithCharacter (fun character ->
            match Character.getActionTypeOpt character with
            | Some actionType ->
                let inputState =
                    match actionType with
                    | Attack -> RegularMenu
                    | Defend -> RegularMenu
                    | Tech _ -> TechMenu
                    | Consume _ -> ItemMenu
                    | Consequence _ | ActionType.Message (_, _) | Wound -> failwithumf ()
                Character.mapCharacterInputState (constant inputState) character
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

    let retargetCharacter sourceIndex targetIndex battle =
        match tryGetCharacter targetIndex battle with
        | Some target when target.Healthy ->
            tryWithCharacter (fun source ->
                if source.Healthy
                then Character.mapAutoBattleOpt (function Some autoBattle -> Some { autoBattle with AutoTarget = targetIndex } | None -> None) source
                else source)
                sourceIndex
                battle
        | Some _ | None -> battle

    (* Evaluation Operations *)

    let evalRetarget affectingWounded targetIndexOpt battle =
        match targetIndexOpt with
        | Some targetIndex ->
            if affectingWounded then
                match tryGetCharacterBy (fun (target : Character) -> target.Healthy) targetIndex battle with
                | Some true | None ->
                    match targetIndex with
                    | AllyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getAlliesWounded battle)))
                    | EnemyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getEnemiesSwooning battle)))
                | Some false -> targetIndexOpt
            else
                match tryGetCharacterBy (fun (target : Character) -> target.Wounded) targetIndex battle with
                | Some true | None ->
                    match targetIndex with
                    | AllyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getAlliesHealthy battle)))
                    | EnemyIndex _ -> Gen.randomItemOpt (Map.toKeyList (Map.remove targetIndex (getEnemiesStanding battle)))
                | Some false -> targetIndexOpt
        | None -> targetIndexOpt

    let evalAttack effectType sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        Character.evalAttack effectType source target

    let evalTechUnary splash targetCount techData sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        (techData.TechCost, Character.evalTechUnary splash targetCount techData source target)

    let evalTech sourceIndex targetIndex techType battle =
        match Map.tryFind techType Data.Value.Techs with
        | Some techData ->
            let source = getCharacter sourceIndex battle
            let target = getCharacter targetIndex battle
            let characters = getCharacters battle
            Triple.prepend techData.TechCost (Character.evalTech techData source target characters)
        | None -> (0, None, Map.empty)

    let rec private evalSingleTargetType targetType (source : Character) (target : Character) (observer : Character) battle =
        match targetType with
        | Self -> observer.CharacterIndex = target.CharacterIndex
        | Other -> observer.CharacterIndex <> target.CharacterIndex
        | SelfOrFriendly -> let friendlies = getFriendlies observer.Ally battle in Map.containsKey target.CharacterIndex friendlies
        | Friendly -> let friendlies = getFriendlies observer.Ally battle in observer.CharacterIndex <> target.CharacterIndex && Map.containsKey target.CharacterIndex friendlies
        | Unfriendly -> let unfriendlies = getUnfriendlies observer.Ally battle in Map.containsKey target.CharacterIndex unfriendlies
        | Type ty -> target.CharacterType = ty
        | BattleTargetType.Any targetTypes -> List.exists (fun targetType -> evalSingleTargetType targetType source target observer battle) targetTypes
        | BattleTargetType.All targetTypes -> List.forall (fun targetType -> evalSingleTargetType targetType source target observer battle) targetTypes

    let rec private evalAttackAffectType affectType (source : Character) (target : Character) (observer : Character) battle =
        match affectType with
        | Physical | Touching -> source.ArchetypeType.AttackTouchingArchetype
        | Magical | Affinity _ | Item | OrbEmptied | OrbFilled | Cancelled | Uncancelled | Buffed | Debuffed -> false
        | Wounded -> target.Wounded
        | Random chance -> Gen.randomf < chance
        | HitPointsLessThanOrEqual ceiling -> target.Healthy && single target.HitPoints / single target.HitPointsMax <= ceiling
        | HitPointsGreaterThanOrEqual floor -> target.Healthy && single target.HitPoints / single target.HitPointsMax >= floor
        | TechPointsLessThanOrEqual ceiling -> single target.TechPoints / single target.TechPointsMax <= ceiling
        | TechPointsGreaterThanOrEqual floor -> single target.TechPoints / single target.TechPointsMax >= floor
        | Any affectTypes -> List.exists (fun affectType -> evalAttackAffectType affectType source target observer battle) affectTypes
        | All affectTypes -> List.forall (fun affectType -> evalAttackAffectType affectType source target observer battle) affectTypes

    let private evalAttackInteractions4 (source : Character) (target : Character) (observer : Character) battle =
        List.fold (fun consequences interaction ->
            let condition = interaction.BattleCondition
            let consequences' = interaction.BattleConsequences
            let satisfied =
                match condition with
                | LastSurviving ->
                    if observer.Healthy then
                        let friendliesHealthy = getFriendliesHealthy observer.Ally battle
                        friendliesHealthy.Count = 1
                    else false
                | LastTypeSurviving ->
                    if observer.Healthy then
                        let friendliesHealthyDifferent = battle |> getFriendliesHealthy observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        friendliesHealthyDifferent.Count = 0
                    else false
                | BecomeLastSurviving ->
                    if observer.Healthy then
                        let friendlies = getFriendlies observer.Ally battle
                        let friendliesHealthy = getFriendliesHealthy observer.Ally battle
                        friendlies.Count > 1 && friendliesHealthy.Count = 1
                    else false
                | BecomeLastTypeSurviving ->
                    if observer.Healthy then
                        let friendliesWoundedDifferent = battle |> getFriendliesWounded observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        let friendliesHealthyDifferent = battle |> getFriendliesHealthy observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        friendliesWoundedDifferent.Count > 0 && friendliesHealthyDifferent.Count = 0
                    else false
                | AffectedTarget (affectType, targetType) ->
                    if observer.Healthy || affectType = Wounded then
                        evalAttackAffectType affectType source target observer battle &&
                        evalSingleTargetType targetType source target observer battle
                    else false
            if satisfied then consequences @ consequences' else consequences)
            [] observer.Interactions

    let evalAttackInteractions sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        let characters = getCharacters battle
        Seq.foldBack (fun (observerIndex, observer : Character) consequences ->
            let consequences' = evalAttackInteractions4 source target observer battle
            (sourceIndex, targetIndex, observerIndex, consequences') :: consequences)
            characters.Pairs []

    let rec private evalItemAffectType affectType (source : Character) (target : Character) (observer : Character) battle =
        match affectType with
        | Physical | Touching | Magical | Affinity _ | OrbEmptied | OrbFilled | Cancelled | Uncancelled | Buffed | Debuffed -> false
        | Item -> true
        | Wounded -> target.Wounded
        | Random chance -> Gen.randomf < chance
        | HitPointsLessThanOrEqual ceiling -> target.Healthy && single target.HitPoints / single target.HitPointsMax <= ceiling
        | HitPointsGreaterThanOrEqual floor -> target.Healthy && single target.HitPoints / single target.HitPointsMax >= floor
        | TechPointsLessThanOrEqual ceiling -> single target.TechPoints / single target.TechPointsMax <= ceiling
        | TechPointsGreaterThanOrEqual floor -> single target.TechPoints / single target.TechPointsMax >= floor
        | Any affectTypes -> List.exists (fun affectType -> evalItemAffectType affectType source target observer battle) affectTypes
        | All affectTypes -> List.forall (fun affectType -> evalItemAffectType affectType source target observer battle) affectTypes

    let private evalItemInteractions4 (source : Character) (target : Character) (observer : Character) battle =
        List.fold (fun consequences interaction ->
            let condition = interaction.BattleCondition
            let consequences' = interaction.BattleConsequences
            let satisfied =
                match condition with
                | LastSurviving ->
                    if observer.Healthy then
                        let friendliesHealthy = getFriendliesHealthy observer.Ally battle
                        friendliesHealthy.Count = 1
                    else false
                | LastTypeSurviving ->
                    if observer.Healthy then
                        let friendliesHealthyDifferent = battle |> getFriendliesHealthy observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        friendliesHealthyDifferent.Count = 0
                    else false
                | BecomeLastSurviving ->
                    if observer.Healthy then
                        let friendlies = getFriendlies observer.Ally battle
                        let friendliesHealthy = getFriendliesHealthy observer.Ally battle
                        friendlies.Count > 1 && friendliesHealthy.Count = 1
                    else false
                | BecomeLastTypeSurviving ->
                    if observer.Healthy then
                        let friendliesWoundedDifferent = battle |> getFriendliesWounded observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        let friendliesHealthyDifferent = battle |> getFriendliesHealthy observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        friendliesWoundedDifferent.Count > 0 && friendliesHealthyDifferent.Count = 0
                    else false
                | AffectedTarget (affectType, targetType) ->
                    if observer.Healthy || affectType = Wounded then
                        evalItemAffectType affectType source target observer battle &&
                        evalSingleTargetType targetType source target observer battle
                    else false
            if satisfied then consequences @ consequences' else consequences)
            [] observer.Interactions

    let evalItemInteractions sourceIndex targetIndex battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        let characters = getCharacters battle
        Seq.foldBack (fun (observerIndex, observer : Character) consequences ->
            let consequences' = evalItemInteractions4 source target observer battle
            (sourceIndex, targetIndex, observerIndex, consequences') :: consequences)
            characters.Pairs []

    let rec private evalTechAffectType affectType techType cancelled affectsWounded delta statusesAdded statusesRemoved (source : Character) (target : Character) (observer : Character) (battle : Battle) =
        match Data.Value.Techs.TryGetValue techType with
        | (true, tech) ->
            match affectType with
            | Physical -> tech.EffectType = EffectType.Physical
            | Magical -> tech.EffectType = EffectType.Magical
            | Touching -> tech.TechType.TouchingTech
            | Affinity affinity -> tech.AffinityOpt = Some affinity
            | Item -> false
            | OrbEmptied -> false
            | OrbFilled -> false
            | Cancelled -> cancelled
            | Uncancelled -> not cancelled && Option.isSome target.AutoBattleOpt
            | Debuffed -> Seq.exists StatusType.debuff statusesAdded
            | Buffed -> Seq.exists StatusType.buff statusesAdded
            | Wounded -> target.Wounded
            | Random chance -> Gen.randomf < chance
            | HitPointsLessThanOrEqual ceiling -> target.Healthy && single target.HitPoints / single target.HitPointsMax <= ceiling
            | HitPointsGreaterThanOrEqual floor -> target.Healthy && single target.HitPoints / single target.HitPointsMax >= floor
            | TechPointsLessThanOrEqual ceiling -> single target.TechPoints / single target.TechPointsMax <= ceiling
            | TechPointsGreaterThanOrEqual floor -> single target.TechPoints / single target.TechPointsMax >= floor
            | Any affectTypes -> List.exists (fun affectType -> evalTechAffectType affectType techType cancelled affectsWounded delta statusesAdded statusesRemoved source target observer battle) affectTypes
            | All affectTypes -> List.forall (fun affectType -> evalTechAffectType affectType techType cancelled affectsWounded delta statusesAdded statusesRemoved source target observer battle) affectTypes
        | (false, _) -> false

    let private evalTechInteractions4 (source : Character) (_ : Character) (observer : Character) (techType : TechType) (techResults : Map<CharacterIndex, bool * bool * int * StatusType Set * StatusType Set>) battle =
        List.fold (fun consequences interaction ->
            let condition = interaction.BattleCondition
            let consequences' = interaction.BattleConsequences
            let satisfied =
                match condition with
                | LastSurviving ->
                    if observer.Healthy then
                        let friendliesHealthy = getFriendliesHealthy observer.Ally battle
                        friendliesHealthy.Count = 1
                    else false
                | LastTypeSurviving ->
                    if observer.Healthy then
                        let friendliesHealthyDifferent = battle |> getFriendliesHealthy observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        friendliesHealthyDifferent.Count = 0
                    else false
                | BecomeLastSurviving ->
                    if observer.Healthy then
                        let friendlies = getFriendlies observer.Ally battle
                        let friendliesHealthy = getFriendliesHealthy observer.Ally battle
                        friendlies.Count > 1 && friendliesHealthy.Count = 1
                    else false
                | BecomeLastTypeSurviving ->
                    if observer.Healthy then
                        let friendliesWoundedDifferent = battle |> getFriendliesWounded observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        let friendliesHealthyDifferent = battle |> getFriendliesHealthy observer.Ally |> Map.filter (fun _ (ally : Character) -> ally.ArchetypeType <> observer.ArchetypeType)
                        friendliesWoundedDifferent.Count > 0 && friendliesHealthyDifferent.Count = 0
                    else false
                | AffectedTarget (affectType, targetType) ->
                    if observer.Healthy || affectType = Wounded then
                        techResults |>
                        Map.map (fun characterIndex result -> (result, tryGetCharacter characterIndex battle)) |>
                        Map.toValueList |>
                        List.exists (fun ((cancelled, affectsWounded, delta, statusesAdded, statusesRemoved), targetOpt) ->
                            match targetOpt with
                            | Some target ->
                                evalTechAffectType affectType techType cancelled affectsWounded delta statusesAdded statusesRemoved source target observer battle &&
                                evalSingleTargetType targetType source target observer battle
                            | None -> false)
                    else false
            if satisfied then consequences @ consequences' else consequences)
            [] observer.Interactions

    let evalTechInteractions sourceIndex targetIndex techType techResults battle =
        let source = getCharacter sourceIndex battle
        let target = getCharacter targetIndex battle
        let characters = getCharacters battle
        Seq.foldBack (fun (observerIndex, observer : Character) consequences ->
            let consequences' = evalTechInteractions4 source target observer techType techResults battle
            (sourceIndex, targetIndex, observerIndex, consequences') :: consequences)
            characters.Pairs []

    let evalConsequences consequences battle =
        let battle =
            (battle, consequences) ||> List.fold (fun battle (sourceIndex, targetIndex, observerIndex, consequences) ->
                (battle, consequences) ||> List.fold (fun battle consequence ->
                    appendActionCommand (ActionCommand.make (Consequence consequence) sourceIndex (Some targetIndex) (Some observerIndex)) battle))
        sortActionCommands battle

    (* Mid-Level Operations *)

    let rec private tryRandomizeEnemy attempts index enemy (layout : Either<unit, (int * StatureType * EnemyType) option> array array) =
        if attempts < 10000 then
            match Data.Value.Characters.TryFind (Enemy enemy) with
            | Some characterData ->
                match Data.Value.Archetypes.TryFind characterData.ArchetypeType with
                | Some archetypeData ->
                    let (w, h) = (layout.Length, layout.[0].Length)
                    let (x, y) =
                        if index = 0 && characterData.Boss
                        then (w / 2, h / 2 - 1) // HACK: put boss enemy 0 in center.
                        else (Gen.random1 w, Gen.random1 h)
                    let stature = archetypeData.Stature
                    match stature with
                    | SmallStature | NormalStature | LargeStature ->
                        if x > 0 && x < w - 1 && y < h - 1 then
                            match
                                (layout.[x-1].[y+1], layout.[x+0].[y+1], layout.[x+1].[y+1],
                                 layout.[x-1].[y+0], layout.[x+0].[y+0], layout.[x+1].[y+0]) with
                            |   (Left (), Left (), Left (),
                                 Left (), Left (), Left ()) ->
                                layout.[x-1].[y+1] <- Right None; layout.[x+0].[y+1] <- Right None; layout.[x+1].[y+1] <- Right None
                                layout.[x-1].[y+0] <- Right None; layout.[x+0].[y+0] <- Right (Some (index, stature, enemy)); layout.[x+1].[y+0] <- Right None
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
                                layout.[x-2].[y+0] <- Right None; layout.[x-1].[y+0] <- Right None; layout.[x+0].[y+0] <- Right (Some (index, stature, enemy)); layout.[x+1].[y+0] <- Right None; layout.[x+2].[y+0] <- Right None
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
        let origin = v2 -288.0f -240.0f // TODO: turn these duplicated vars into global consts.
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
                    | Right (Some (enemyIndex, enemyStature, enemy)) ->
                        let position =
                            match enemyStature with
                            | SmallStature | NormalStature | LargeStature -> v3 (origin.X + single x * tile.X) (origin.Y + single y * tile.Y) 0.0f
                            | BossStature -> v3 (origin.X + single x * tile.X - 90.0f) (origin.Y + single y * tile.Y) 0.0f
                        Character.tryMakeEnemy allyCount enemyIndex waitSpeed true position enemy)
                    arr) |>
            Array.concat |>
            Array.definitize |>
            Array.toList
        enemies

    let spawnEnemies spawnTypes battle =
        let origin = v2 -288.0f -240.0f // TODO: turn these duplicated vars into global consts.
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
                    let bottom = position + v3 72.0f 0.f 0.0f // HACK: assume spawning character has 144.0f width.
                    let bottoms = battle |> getEnemies |> Map.toValueArray |> Array.map (fun (enemy : Character) -> enemy.PerimeterOriginal.Bottom)
                    let notOnSides = i <> 0 && i <> w - 1
                    let notOverlapping = Array.notExists (fun bottom' -> Vector3.Distance (bottom, bottom') < tile.X * 1.5f) bottoms
                    if notOnSides && notOverlapping then
                        let enemyIndex = Option.mapOrDefaultValue EnemyIndex (nextEnemyIndex battle) spawnType.EnemyIndexOpt
                        let enemyPosition = Option.defaultValue position spawnType.PositionOpt
                        match Character.tryMakeEnemy allyCount enemyIndex.Subindex waitSpeed spawnType.ActionTimeAdvanced enemyPosition spawnType.EnemyType with
                        | Some enemy ->
                            let enemy =
                                match spawnType.SpawnEffectType with
                                | Materialize -> Character.materialize battle.BattleTime_ enemy
                                | Unearth -> Character.animate battle.BattleTime_ UnearthAnimation enemy
                                | Pop -> enemy
                            battle <- addCharacter enemyIndex enemy battle
                            spawned <- true
                        | None -> ()
                    tries <- inc tries
                battle)
                battle
                spawnTypes
        battle

    let spawnEnemy spawnType battle =
        spawnEnemies [spawnType] battle

    (* High-Level Operations (signal-producing) *)

    let private updateAttack sourceIndex (targetIndexOpt : CharacterIndex option) localTime battle =
        if getCharacterHealthy sourceIndex battle then
            match targetIndexOpt with
            | Some targetIndex ->
                if containsCharacter targetIndex battle then
                    match localTime with
                    | 0L ->
                        if getCharacterHealthy targetIndex battle then
                            let sourcePerimeter = getCharacterPerimeter sourceIndex battle
                            let targetPerimeter = getCharacterPerimeter targetIndex battle
                            let battle =
                                if sourcePerimeter.Bottom.X < targetPerimeter.Bottom.X then faceCharacter Rightward sourceIndex battle
                                elif sourcePerimeter.Bottom.X > targetPerimeter.Bottom.X then faceCharacter Leftward sourceIndex battle
                                else battle
                            let battle = animateCharacter AttackAnimation sourceIndex battle
                            let playHit = PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                            withSignal playHit battle
                        else just (abortCharacterInteraction sourceIndex battle)
                    | 15L ->
                        let damage = evalAttack EffectType.Physical sourceIndex targetIndex battle
                        let battle = mapCharacterHitPoints true false false -damage targetIndex battle
                        let battle = animateCharacter DamageAnimation targetIndex battle
                        let battle =
                            if getCharacterWounded targetIndex battle then
                                let battle = halveCharacterActionTime targetIndex battle
                                resetCharacterInput targetIndex battle
                            else battle
                        withSignal (DisplayHitPointsChange (targetIndex, -damage)) battle
                    | _ when localTime > 15L && getCharacterAnimationFinished targetIndex battle ->
                        if getCharacterHealthy targetIndex battle then
                            let battle = animationCharacterPoise sourceIndex battle
                            let battle = animationCharacterPoise targetIndex battle
                            let battle = finishCharacterInteraction sourceIndex battle
                            let battle = mapCurrentCommandOpt (constant None) battle
                            let battle =
                                if shouldCharacterCounter targetIndex sourceIndex battle
                                then characterCounterAttack targetIndex sourceIndex battle
                                else battle
                            let consequences = evalAttackInteractions sourceIndex targetIndex battle
                            let battle = evalConsequences consequences battle
                            just battle
                        else
                            let woundCommand = CurrentCommand.make battle.BattleTime_ (ActionCommand.make Wound sourceIndex (Some targetIndex) None)
                            let battle = animationCharacterPoise sourceIndex battle
                            let battle = finishCharacterInteraction sourceIndex battle
                            let battle = mapCurrentCommandOpt (constant (Some woundCommand)) battle
                            let consequences = evalAttackInteractions sourceIndex targetIndex battle
                            let battle = evalConsequences consequences battle
                            just battle
                    | _ -> just battle
                else just (abortCharacterInteraction sourceIndex battle)
            | None -> just (abortCharacterInteraction sourceIndex battle)
        else just (abortCharacterInteraction sourceIndex battle)

    let private updateDefend sourceIndex localTime battle =
        if getCharacterHealthy sourceIndex battle then
            match localTime with
            | 0L ->
                let battle =
                    battle |>
                    resetCharacterActionTime sourceIndex |>
                    resetCharacterInput sourceIndex |>
                    animateCharacter (PoiseAnimation Defending) sourceIndex |>
                    defendCharacter sourceIndex
                let battle = finishCharacterInteraction sourceIndex battle
                let battle = mapCurrentCommandOpt (constant None) battle
                just battle
            | _ -> just battle
        else
            let battle = mapCurrentCommandOpt (constant None) battle
            just battle

    let private updateConsume consumable sourceIndex (targetIndexOpt : CharacterIndex option) localTime battle =
        if containsCharacterHealthy sourceIndex battle then
            match targetIndexOpt with
            | Some targetIndex ->
                if containsCharacter targetIndex battle then
                    match localTime with
                    | 0L ->
                        if getCharacterHealthy targetIndex battle || consumable = Revive then // HACK: should really be checked ConsumableData.
                            let sourcePerimeter = getCharacterPerimeter sourceIndex battle
                            let targetPerimeter = getCharacterPerimeter targetIndex battle
                            let battle =
                                if sourcePerimeter.Bottom.X < targetPerimeter.Bottom.X then faceCharacter Rightward sourceIndex battle
                                elif sourcePerimeter.Bottom.X > targetPerimeter.Bottom.X then faceCharacter Leftward sourceIndex battle
                                else battle
                            let battle = animateCharacter CastAnimation sourceIndex battle
                            let battle = mapInventory (Inventory.tryRemoveItem (Consumable consumable) >> snd) battle
                            just battle
                        else just (abortCharacterInteraction sourceIndex battle)
                    | 30L ->
                        match Data.Value.Consumables.TryGetValue consumable with
                        | (true, consumableData) ->
                            if consumableData.Curative then
                                let healing = int consumableData.Scalar
                                let battle =
                                    if consumableData.Techative
                                    then mapCharacterTechPoints healing targetIndex battle
                                    else mapCharacterHitPoints true false consumableData.Revive healing targetIndex battle
                                let battle = applyCharacterStatuses consumableData.StatusesAdded consumableData.StatusesRemoved targetIndex battle
                                let battle = animateCharacter SpinAnimation targetIndex battle
                                let displayHitPointsChange = DisplayHitPointsChange (targetIndex, healing)
                                let playHealSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.HealSound)
                                withSignals [displayHitPointsChange; playHealSound] battle
                            else just battle // TODO: non-curative case
                        | (false, _) -> just battle
                    | _ when localTime > 30L && getCharacterAnimationFinished targetIndex battle ->
                        let battle = animationCharacterPoise sourceIndex battle
                        let battle = animationCharacterPoise targetIndex battle
                        let battle = finishCharacterInteraction sourceIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        let consequences = evalItemInteractions sourceIndex targetIndex battle
                        let battle = evalConsequences consequences battle
                        just battle
                    | _ -> just battle
                else just (abortCharacterInteraction sourceIndex battle)
            | None -> just (abortCharacterInteraction sourceIndex battle)
        else just (abortCharacterInteraction sourceIndex battle)

    let private updateMessage text lifeTime localTime battle =
        ignore<int64> localTime
        let dialog = Dialog.make DialogShort text
        let battle = mapMessageOpt (constant (Some (battle.BattleTime_, lifeTime, dialog))) battle
        let battle = mapCurrentCommandOpt (constant None) battle
        just battle

    let private updateTech techType sourceIndex (targetIndexOpt : CharacterIndex option) localTime battle =
        if containsCharacterHealthy sourceIndex battle then
            match targetIndexOpt with
            | Some targetIndex ->
                if containsCharacter targetIndex battle then
                    match (Map.tryFind techType Data.Value.Techs, Map.tryFind techType Data.Value.TechAnimations) with
                    | (Some techData, Some techAnimationData) ->
                        ignore techData // TODO: check for target.IsWounded case if techData is affecting wounded...
                        if getCharacterHealthy targetIndex battle then
                            let (sigs, battle) =
                                if localTime = techAnimationData.TechStart then
                                    let sourcePerimeter = getCharacterPerimeter sourceIndex battle
                                    let targetPerimeter = getCharacterPerimeter targetIndex battle
                                    let battle =
                                        if sourcePerimeter.Bottom.X < targetPerimeter.Bottom.X then faceCharacter Rightward sourceIndex battle
                                        elif sourcePerimeter.Bottom.X > targetPerimeter.Bottom.X then faceCharacter Leftward sourceIndex battle
                                        else battle
                                    let effectOpt =
                                        match techType with
                                        | Cyclone ->
                                            Left (DisplayHop (sourcePerimeter.Bottom, targetPerimeter.Bottom + Constants.Battle.CharacterBottomOffset3))
                                        | _ when techType.TouchingTech ->
                                            let hopDirection = Direction.ofVector3 (v3 (targetPerimeter.Bottom.X - sourcePerimeter.Bottom.X) 0.0f 0.0f)
                                            let hopStop = targetPerimeter.Bottom - Direction.toVector3 hopDirection * Constants.Battle.StrikingDistance
                                            Left (DisplayHop (sourcePerimeter.Bottom, hopStop))
                                        | _ ->
                                            match getCharacterArchetypeType sourceIndex battle with
                                            | Cleric ->
                                                let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeHolySound)
                                                let displayCast = DisplayHolyCast (0L, sourceIndex)
                                                Right [signal playCharge; signal displayCast]
                                            | Wizard ->
                                                let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                                                let displayCast = DisplayArcaneCast (0L, sourceIndex)
                                                Right [playCharge; displayCast]
                                            | Conjuror ->
                                                let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                                                let displayCast = DisplayDimensionalCast (0L, sourceIndex)
                                                Right [playCharge; displayCast]
                                            | _ ->
                                                let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                                                let displayCast = DisplayGenericCast (0L, sourceIndex)
                                                Right [playCharge; displayCast]
                                    match effectOpt with
                                    | Left hopEffect ->
                                        let battle = animateCharacter (PoiseAnimation Poising) sourceIndex battle
                                        withSignal hopEffect battle
                                    | Right chargeEffects ->
                                        if getCharacterHealthy targetIndex battle then
                                            let battle = animateCharacter (PoiseAnimation Charging) sourceIndex battle
                                            withSignals chargeEffects battle
                                        else just (abortCharacterInteraction sourceIndex battle)
                                elif localTime = techAnimationData.TechingStart then
                                    match techType with
                                    | Critical ->
                                        let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let critical = DisplayCritical (13L, targetIndex)
                                        let displayCut = DisplayCut (20L, true, targetIndex)
                                        let battle = animateCharacter AttackAnimation sourceIndex battle
                                        withSignals [playHit; critical; displayCut] battle
                                    | Slash ->
                                        let playSlash = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.SlashSound)
                                        let playHit = PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let perimeter = getCharacterPerimeter sourceIndex battle
                                        let slashSpike = DisplaySlashSpike (10L, perimeter.Bottom, targetIndex)
                                        let impactSplashes = evalTech sourceIndex targetIndex techType battle |> Triple.thd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayImpactSplash (70L, targetIndex) |> signal)
                                        let battle = animateCharacter SlashAnimation sourceIndex battle
                                        withSignals (playSlash :: playHit :: slashSpike :: impactSplashes) battle
                                    | HeavyCritical ->
                                        let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let heavyCritical = DisplayHeavyCritical (10L, targetIndex)
                                        let displayCut = DisplayCut (20L, true, targetIndex)
                                        let impactSplash = DisplayImpactSplash (34L, targetIndex)
                                        let battle = animateCharacter AttackAnimation sourceIndex battle
                                        withSignals [playHit; heavyCritical; displayCut; impactSplash] battle
                                    | Cyclone ->
                                        let radius = 64.0f
                                        let perimeter = getCharacterPerimeter sourceIndex battle
                                        let position = perimeter.Bottom
                                        let playHits =
                                            [PlaySound (20L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound) |> signal
                                             PlaySound (40L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                             PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                             PlaySound (80L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)]
                                        let sigs =
                                            signal (DisplayCircle (position, radius)) ::
                                            signal (DisplayCycloneBlur (0L, sourceIndex, radius)) ::
                                            playHits
                                        let battle = animateCharacter WhirlAnimation sourceIndex battle
                                        withSignals sigs battle
                                    | CriticalSlash ->
                                        let playSlash = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.TwisterSound)
                                        let playHit = PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let perimeter = getCharacterPerimeter sourceIndex battle
                                        let slashTwister = DisplaySlashTwister (10L, perimeter.Bottom, targetIndex)
                                        let impactSplashes = evalTech sourceIndex targetIndex techType battle |> Triple.thd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayImpactSplash (70L, targetIndex) |> signal)
                                        let battle = animateCharacter SlashAnimation sourceIndex battle
                                        withSignals (playSlash :: playHit :: slashTwister :: impactSplashes) battle
                                    | PoisonCut ->
                                        let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let displayCut = DisplayCut (20L, false, targetIndex)
                                        let poisonCut = DisplayPoisonCut (25L, targetIndex)
                                        let battle = animateCharacter AttackAnimation sourceIndex battle
                                        withSignals [playHit; displayCut; poisonCut] battle
                                    | PowerCut ->
                                        let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let displayCut = DisplayCut (20L, false, targetIndex)
                                        let powerCut = DisplayPowerCut (20L, targetIndex)
                                        let battle = animateCharacter AttackAnimation sourceIndex battle
                                        withSignals [playHit; displayCut; powerCut] battle
                                    | DispelCut ->
                                        let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let displayCut = DisplayCut (20L, false, targetIndex)
                                        let dispelCut = DisplayDispelCut (25L, targetIndex)
                                        let battle = animateCharacter AttackAnimation sourceIndex battle
                                        withSignals [playHit; displayCut; dispelCut] battle
                                    | DoubleCut ->
                                        let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let playHit2 = PlaySound (20L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                                        let displayCut = DisplayCut (20L, false, targetIndex)
                                        let doubleCut = DisplayDoubleCut (20L, targetIndex)
                                        let battle = animateCharacter AttackAnimation sourceIndex battle
                                        withSignals [playHit; playHit2; displayCut; doubleCut] battle
                                    | Fire ->
                                        let playFire = PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.FireSound)
                                        let displayFire = DisplayFire (0L, sourceIndex, targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playFire; displayFire] battle
                                    | TechType.Flame ->
                                        let playFlame = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.FlameSound)
                                        let displayFlame = DisplayFlame (0L, sourceIndex, targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playFlame; displayFlame] battle
                                    | Ice ->
                                        let playIce = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.IceSound)
                                        let displayIce = DisplayIce (0L, targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playIce; displayIce] battle
                                    | Snowball ->
                                        let playSnowball = PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.SnowballSound)
                                        let displaySnowball = DisplaySnowball (0L, targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playSnowball; displaySnowball] battle
                                    | Cure ->
                                        let playCure = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.CureSound)
                                        let displayCures = evalTech sourceIndex targetIndex techType battle |> Triple.thd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayCure (0L, targetIndex) |> signal)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals (signal playCure :: displayCures) battle
                                    | Empower ->
                                        let playBuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                                        let displayBuff = DisplayBuff (0L, Power (true, true), targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playBuff; displayBuff] battle
                                    | Aura ->
                                        let playCure = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.CureSound)
                                        let displayCures = evalTech sourceIndex targetIndex techType battle |> Triple.thd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayCure (0L, targetIndex) |> signal)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals (signal playCure :: displayCures) battle
                                    | Enlighten ->
                                        let playBuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                                        let displayBuff = DisplayBuff (0L, Magic (true, true), targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playBuff; displayBuff] battle
                                    | Protect ->
                                        let playBuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                                        let displayBuff = DisplayBuff (0L, Shield (true, true), targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playBuff; displayBuff] battle
                                    | Purify ->
                                        let displayPurify = DisplayPurify (0L, targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignal displayPurify battle
                                    | Muddle ->
                                        let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                                        let displayDebuff = DisplayDebuff (0L, Magic (false, false), targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playDebuff; displayDebuff] battle
                                    | Weaken ->
                                        let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                                        let displayDebuff = DisplayDebuff (0L, Power (false, false), targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playDebuff; displayDebuff] battle
                                    | Slow ->
                                        let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                                        let displayDebuff = DisplayDebuff (0L, Time false, targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playDebuff; displayDebuff] battle
                                    | Bolt ->
                                        let playSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ExplosionSound)
                                        let displayBolt = DisplayBolt (0L, targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playSound; displayBolt] battle
                                    | ConjureRamuh ->
                                        let playThunders =
                                            [PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ThunderSound) |> signal
                                             PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.ExplosionSound)
                                             PlaySound (30L, Constants.Audio.SoundVolumeDefault, Assets.Field.ExplosionSound)
                                             PlaySound (45L, Constants.Audio.SoundVolumeDefault, Assets.Field.ExplosionSound)]
                                        let displayScatterBolts =
                                            [DisplayScatterBolt 0L |> signal
                                             DisplayScatterBolt 15L
                                             DisplayScatterBolt 30L
                                             DisplayScatterBolt 45L]
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals (playThunders @ displayScatterBolts) battle
                                    | Inferno ->
                                        let playInferno = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.InfernoSound)
                                        let displayInferno = DisplayInferno 0L
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playInferno; displayInferno] battle
                                    | Silk ->
                                        let playSilk = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SilkSound)
                                        let displaySilk = DisplaySilk (0L, targetIndex)
                                        let battle = animateCharacter Cast2Animation sourceIndex battle
                                        withSignals [playSilk; displaySilk] battle
                                elif localTime = techAnimationData.AffectingStart then
                                    let (_, spawnOpt, results) = evalTech sourceIndex targetIndex techType battle
                                    let (battle, sigs) =
                                        Map.fold (fun (battle, sigs) characterIndex (cancelled, _, hitPointsChange, _, _) ->
                                            if hitPointsChange < 0 && getCharacterHealthy characterIndex battle then
                                                let battle = animateCharacter DamageAnimation characterIndex battle
                                                let displayCancel = DisplayCancel characterIndex
                                                let sigs = if cancelled then signal displayCancel :: sigs else sigs
                                                (battle, sigs)
                                            else (battle, sigs))
                                            (battle, [])
                                            results
                                    let battle =
                                        match spawnOpt with
                                        | Some spawn -> spawnEnemies spawn battle
                                        | _ -> battle
                                    withSignals sigs battle
                                elif localTime = techAnimationData.AffectingStop then
                                    let results = evalTech sourceIndex targetIndex techType battle |> Triple.thd
                                    let (battle, sigs) =
                                        Map.fold (fun (battle, sigs) _ (_, _, _, _, _) ->
                                            // TODO: emission effect
                                            (battle, sigs))
                                            (battle, [])
                                            results
                                    withSignals sigs battle
                                elif localTime = techAnimationData.TechingStop then
                                    let sourcePerimeterOriginal = getCharacterPerimeterOriginal sourceIndex battle
                                    let targetPerimeter = getCharacterPerimeter targetIndex battle
                                    let hopOpt =
                                        match techType with
                                        | Cyclone ->
                                            Some (targetPerimeter.Bottom + Constants.Battle.CharacterBottomOffset3, sourcePerimeterOriginal.Bottom)
                                        | _ when techType.TouchingTech ->
                                            let hopDirection = Direction.ofVector3 (v3 (targetPerimeter.Bottom.X - sourcePerimeterOriginal.Bottom.X) 0.0f 0.0f)
                                            let hopStart = targetPerimeter.Bottom - Direction.toVector3 hopDirection * Constants.Battle.StrikingDistance
                                            Some (hopStart, sourcePerimeterOriginal.Bottom)
                                        | _ -> None
                                    match hopOpt with
                                    | Some (hopStart, hopStop) -> withSignal (DisplayHop (hopStart, hopStop)) battle
                                    | None -> just battle
                                elif localTime > techAnimationData.TechStop then
                                    let battle = if techData.SpawnOpt.IsSome then finalizeMaterializations battle else battle
                                    let (techCost, _, results) = evalTech sourceIndex targetIndex techType battle
                                    let source = getCharacter sourceIndex battle
                                    let (battle, sigs) =
                                        Map.fold (fun (battle, sigs) characterIndex (cancelled, affectsWounded, hitPointsChange, added, removed) ->
                                            let battle = mapCharacterHitPoints true cancelled affectsWounded hitPointsChange characterIndex battle
                                            let vulnerabilities = getCharacterVulnerabilities characterIndex battle
                                            let randomizer = if sourceIndex.Ally then StatusType.randomizeStrong vulnerabilities else StatusType.randomizeWeak vulnerabilities
                                            let added = added |> Set.toSeq |> Seq.filter randomizer |> Set.ofSeq
                                            let battle = applyCharacterStatuses added removed characterIndex battle
                                            let wounded = getCharacterWounded characterIndex battle
                                            let battle =
                                                if wounded then
                                                    let battle = halveCharacterActionTime characterIndex battle
                                                    resetCharacterInput characterIndex battle
                                                else battle
                                            let sigs =
                                                if hitPointsChange <> 0 then
                                                    let displayHpc = DisplayHitPointsChange (characterIndex, hitPointsChange) |> signal
                                                    displayHpc :: sigs
                                                else sigs
                                            let (battle, sigs) =
                                                if wounded then
                                                    let woundCommand = ActionCommand.make Wound sourceIndex (Some characterIndex) None
                                                    let battle = prependActionCommand woundCommand battle
                                                    (battle, sigs)
                                                else
                                                    let battle = animationCharacterPoise characterIndex battle
                                                    (battle, sigs)
                                            (battle, sigs))
                                            (battle, [])
                                            results
                                    let battle = mapCharacterTechPoints -techCost sourceIndex battle
                                    let battle = animationCharacterPoise sourceIndex battle
                                    let battle =
                                        match source.TechChargeOpt with
                                        | Some (_, _, chargeTechType) when techType = chargeTechType -> resetCharacterTechCharge sourceIndex battle
                                        | Some _ | None -> battle
                                    let battle =
                                        if techType.ConjureTech
                                        then resetCharacterConjureCharge sourceIndex battle
                                        else battle
                                    let battle = finishCharacterInteraction sourceIndex battle
                                    let battle = mapCurrentCommandOpt (constant None) battle
                                    let battle =
                                        if shouldCharacterCounter targetIndex sourceIndex battle
                                        then characterCounterAttack targetIndex sourceIndex battle
                                        else battle
                                    let consequences = evalTechInteractions sourceIndex targetIndex techType results battle
                                    let battle = evalConsequences consequences battle
                                    withSignals sigs battle
                                else just battle
                            withSignals sigs battle
                        else just (abortCharacterInteraction sourceIndex battle)
                    | (_, _) -> just (abortCharacterInteraction sourceIndex battle)
                else just (abortCharacterInteraction sourceIndex battle)
            | None -> just (abortCharacterInteraction sourceIndex battle)
        else just (abortCharacterInteraction sourceIndex battle)

    let private updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt consequence messageOpt localTime battle =
        let messageTime = 45L
        match messageOpt with
        | Some message ->
            if localTime = 0L then
                let dialog = Dialog.make DialogShort message
                let battle = mapMessageOpt (constant (Some (battle.BattleTime_, dec messageTime, dialog))) battle
                (false, battle)
            else
                let actionCommand = { Action = Consequence consequence; SourceIndex = sourceIndex; TargetIndexOpt = targetIndexOpt; ObserverIndexOpt = observerIndexOpt }
                let currentCommand = { StartTime = battle.BattleTime_; ActionCommand = actionCommand }
                let battle = mapCurrentCommandOpt (constant (Some currentCommand)) battle
                (false, battle)
        | None -> (true, battle)

    let private updateConsequence sourceIndex targetIndexOpt observerIndexOpt consequence localTime (battle : Battle) =
        match (targetIndexOpt, observerIndexOpt) with
        | (Some targetIndex, Some observerIndex) ->
            match consequence with
            | Charge (chargeAmount, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (Charge (chargeAmount, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = chargeCharacter chargeAmount observerIndex battle
                        let battle =
                            match getCharacterBy (fun c -> c.TechChargeOpt) observerIndex battle with
                            | Some (_, chargeAmount, techType) when chargeAmount >= Constants.Battle.ChargeMax ->
                                let battle = mapCharacterAutoTechOpt (constant (Some techType)) observerIndex battle
                                let battle = retargetCharacter observerIndex sourceIndex battle // TODO: make this target self if healing tech.
                                battle
                            | Some _ | None -> battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | AddVulnerability (vulnerabilityType, vulnerabilityRank, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (AddVulnerability (vulnerabilityType, vulnerabilityRank, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = applyCharacterVulnerabilities (Map.singleton vulnerabilityType vulnerabilityRank) Set.empty observerIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | RemoveVulnerability (vulnerabilityType, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (RemoveVulnerability (vulnerabilityType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = applyCharacterVulnerabilities Map.empty (Set.singleton vulnerabilityType) observerIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | AddStatus (statusType, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (AddStatus (statusType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = applyCharacterStatuses (Set.singleton statusType) Set.empty observerIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | RemoveStatus (statusType, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (RemoveStatus (statusType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = applyCharacterStatuses Set.empty (Set.singleton statusType) observerIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | CounterAttack messageOpt ->
                if containsCharacterHealthy sourceIndex battle && containsCharacterHealthy observerIndex battle && not ((getCharacter observerIndex battle).Statuses.ContainsKey Sleep) then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (CounterAttack None) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = prependActionCommand (ActionCommand.make Attack observerIndex (Some sourceIndex) None) battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | CounterTech (techType, messageOpt) ->
                if containsCharacterHealthy sourceIndex battle && containsCharacterHealthy observerIndex battle && not ((getCharacter observerIndex battle).Statuses.ContainsKey Sleep) && not ((getCharacter observerIndex battle).Statuses.ContainsKey Silence) then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (CounterTech (techType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = prependActionCommand (ActionCommand.make (Tech techType) observerIndex (Some sourceIndex) None) battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | CounterConsumable (consumableType, messageOpt) ->
                if containsCharacterHealthy sourceIndex battle && containsCharacterHealthy observerIndex battle && not ((getCharacter observerIndex battle).Statuses.ContainsKey Sleep) then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (CounterConsumable (consumableType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = prependActionCommand (ActionCommand.make (Consume consumableType) observerIndex (Some sourceIndex) None) battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | AssistTech (techType, messageOpt) ->
                if containsCharacterHealthy targetIndex battle && containsCharacterHealthy observerIndex battle && not ((getCharacter observerIndex battle).Statuses.ContainsKey Sleep) then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (AssistTech (techType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = prependActionCommand (ActionCommand.make (Tech techType) observerIndex (Some targetIndex) None) battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | AssistConsumable (consumableType, messageOpt) ->
                if containsCharacterHealthy targetIndex battle && containsCharacterHealthy observerIndex battle && not ((getCharacter observerIndex battle).Statuses.ContainsKey Sleep) then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (AssistConsumable (consumableType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = prependActionCommand (ActionCommand.make (Consume consumableType) observerIndex (Some targetIndex) None) battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | PilferGold (gold, messageOpt) ->
                if containsCharacterHealthy observerIndex battle && not ((getCharacter observerIndex battle).Statuses.ContainsKey Sleep) then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (PilferGold (gold, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = mapInventory (Inventory.removeGold gold) battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | PilferConsumable (consumableType, messageOpt) ->
                if containsCharacterHealthy observerIndex battle && not ((getCharacter observerIndex battle).Statuses.ContainsKey Sleep) then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (PilferConsumable (consumableType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = mapInventory (Inventory.tryRemoveItem (Consumable consumableType) >> snd) battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | RetargetToSource messageOpt ->
                if containsCharacterHealthy sourceIndex battle && containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (RetargetToSource None) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = retargetCharacter observerIndex sourceIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | RetargetFriendliesToSource messageOpt ->
                if containsCharacterHealthy sourceIndex battle && containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (RetargetFriendliesToSource None) messageOpt localTime battle with
                    | (true, battle) ->
                        let friendlies = getFriendlies observerIndex.Ally battle
                        let battle = Map.fold (fun battle friendlyIndex _ -> retargetCharacter friendlyIndex sourceIndex battle) battle friendlies
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | ChangeAction (techTypeOpt, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (ChangeAction (techTypeOpt, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = mapCharacterAutoTechOpt (constant techTypeOpt) observerIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | ChangeFriendlyActions (techTypeOpt, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (ChangeFriendlyActions (techTypeOpt, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let friendlies = getFriendlies observerIndex.Ally battle
                        let battle = Map.fold (fun battle friendlyIndex _ -> mapCharacterAutoTechOpt (constant techTypeOpt) friendlyIndex battle) battle friendlies
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | Duplicate messageOpt ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (Duplicate None) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle =
                            match (getCharacter observerIndex battle).CharacterType with
                            | Enemy enemyType -> spawnEnemies [{ EnemyType = enemyType; SpawnEffectType = Materialize; ActionTimeAdvanced = false; PositionOpt = None; EnemyIndexOpt = None }] battle
                            | Ally _ -> battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | AddBattleInteraction (interaction, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (AddBattleInteraction (interaction, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = addCharacterInteraction interaction observerIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | ClearBattleInteractions messageOpt ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (ClearBattleInteractions None) messageOpt localTime battle with
                    | (true, battle) ->
                        let battle = clearCharacterInteractions observerIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | Replace (enemyType, messageOpt) ->
                if containsCharacterHealthy observerIndex battle then
                    match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (Replace (enemyType, None)) messageOpt localTime battle with
                    | (true, battle) ->
                        if localTime = 0L then
                            let battle = animateCharacter ReadyAnimation observerIndex battle
                            let battle = dematerializeCharacter observerIndex battle
                            just battle
                        elif localTime = Constants.Battle.CharacterDematerializeDuration then
                            let spawnPosition = (getCharacterPerimeter observerIndex battle).BottomLeft
                            let spawnType = { EnemyType = enemyType; SpawnEffectType = Materialize; ActionTimeAdvanced = false; PositionOpt = Some spawnPosition; EnemyIndexOpt = Some observerIndex.Subindex }
                            let battle = removeCharacter observerIndex battle
                            let battle = spawnEnemy spawnType battle
                            let battle = animateCharacter WalkAnimation observerIndex battle
                            let battle = faceCharacter Downward observerIndex battle
                            just battle
                        elif localTime = Constants.Battle.CharacterDematerializeDuration + Constants.Battle.CharacterMaterializeDuration then
                            let battle = animateCharacter (PoiseAnimation Poising) observerIndex battle
                            let battle = mapCurrentCommandOpt (constant None) battle
                            just battle
                        else just battle
                    | (false, battle) -> just battle
                else just (mapCurrentCommandOpt (constant None) battle)
            | Spawn (spawnTypes, messageOpt) ->
                match updateConsequenceMessageOpt sourceIndex targetIndexOpt observerIndexOpt (Spawn (spawnTypes, None)) messageOpt localTime battle with
                | (true, battle) ->
                    if localTime = 0L then
                        let battle = spawnEnemies spawnTypes battle
                        just battle
                    elif localTime = Constants.Battle.CharacterMaterializeDuration then
                        let battle = finalizeMaterializations battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    else just battle
                | (false, battle) -> just battle
            | Message (text, lifeTime) ->
                let battle =
                    if containsCharacterHealthy observerIndex battle then 
                        let lifeTime = if lifeTime <= 0L then 60L else lifeTime
                        let dialog = Dialog.make DialogShort text
                        mapMessageOpt (constant (Some (battle.BattleTime_, lifeTime, dialog))) battle
                    else battle
                let battle = mapCurrentCommandOpt (constant None) battle
                just battle
        | (_, _) -> battle |> mapCurrentCommandOpt (constant None) |> just

    let rec private updateWound targetIndexOpt battle =
        match targetIndexOpt with
        | Some targetIndex ->
            let character = getCharacter targetIndex battle
            let (sigs, battle) =
                if character.Ally then
                    match character.CharacterAnimationType with
                    | DamageAnimation ->
                        if Character.getAnimationFinished battle.BattleTime_ character then
                            let battle = animateCharacterWound targetIndex battle
                            let battle = mapCurrentCommandOpt (constant None) battle
                            just battle
                        else just battle
                    | PoiseAnimation _ -> // allies don't have a wound animation state but rather return to poise state
                        let battle = animateCharacterWound targetIndex battle
                        let battle = mapCurrentCommandOpt (constant None) battle
                        just battle
                    | _ -> failwithumf ()
                else
                    match character.CharacterAnimationType with
                    | DamageAnimation ->
                        if Character.getAnimationFinished battle.BattleTime_ character then
                            let battle = animateCharacterWound targetIndex battle
                            let playDeathSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastDeathSound)
                            withSignal playDeathSound battle
                        else just battle
                    | WoundAnimation ->
                        if Character.getAnimationFinished battle.BattleTime_ character then
                            let battle = if targetIndex.Enemy then removeCharacter targetIndex battle else battle
                            let battle = mapCurrentCommandOpt (constant None) battle
                            just battle
                        else just battle
                    | _ -> failwithumf ()
            let (sigs, battle) =
                match battle.CurrentCommandOpt_ with
                | None ->
                    let allies = battle |> getAllies |> Map.toValueList
                    let enemies = battle |> getEnemies |> Map.toValueList
                    if List.forall (fun (character : Character) -> character.Wounded) allies then
                        // lost battle
                        let battle = animateCharactersCelebrate false battle
                        let battle = mapBattleState (constant (BattleQuitting (battle.BattleTime_, false, Set.empty))) battle
                        let (sigs2, battle) = update battle
                        (sigs @ sigs2, battle)
                    elif List.isEmpty enemies then
                        // won battle
                        let battle = animateCharactersCelebrate true battle
                        let battle = mapBattleState (constant (BattleResult (battle.BattleTime_, true))) battle
                        let (sigs2, battle) = update battle
                        (sigs @ sigs2, battle)
                    else (sigs, battle)
                | Some _ -> (sigs, battle)
            withSignals sigs battle
        | None -> just battle

    and private updateReady startTime (battle : Battle) =
        let localTime = battle.BattleTime_ - startTime
        if localTime = 0L then // first frame after transitioning in
            match battle.BattleSongOpt_ with
            | Some battleSong -> withSignal (PlaySong (0L, Constants.Audio.FadeOutTimeDefault, 0L, Constants.Audio.SongVolumeDefault, battleSong)) battle
            | None -> just battle
        elif localTime >= 30L && localTime < 100L then
            let battle = animateCharactersReady battle
            if localTime = 60L
            then withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.UnsheatheSound)) battle
            else just battle
        elif localTime = 100L then
            let battle = mapBattleState (constant BattleRunning) battle
            let battle = animateCharactersPoised battle
            let battle = populateAlliesConjureCharges battle
            let battle = autoBattleEnemies battle
            just battle
        else just battle

    and private updateCurrentCommand currentCommand battle =
        let localTime = battle.BattleTime_ - currentCommand.StartTime
        let sourceIndex = currentCommand.ActionCommand.SourceIndex
        let targetIndexOpt = currentCommand.ActionCommand.TargetIndexOpt
        let observerIndexOpt = currentCommand.ActionCommand.ObserverIndexOpt
        match currentCommand.ActionCommand.Action with
        | Attack -> updateAttack sourceIndex targetIndexOpt localTime battle
        | Defend -> updateDefend sourceIndex localTime battle
        | Tech techType -> updateTech techType sourceIndex targetIndexOpt localTime battle
        | Consume consumable -> updateConsume consumable sourceIndex targetIndexOpt localTime battle
        | Consequence consequence -> updateConsequence sourceIndex targetIndexOpt observerIndexOpt consequence localTime battle
        | ActionType.Message (text, lifeTime) -> updateMessage text lifeTime localTime battle
        | Wound -> updateWound targetIndexOpt battle

    and private updateNextCommand nextCommand futureCommands battle =
        let command = CurrentCommand.make battle.BattleTime_ nextCommand
        let sourceIndex = command.ActionCommand.SourceIndex
        let targetIndexOpt = command.ActionCommand.TargetIndexOpt
        let observerIndexOpt = command.ActionCommand.ObserverIndexOpt
        let source = getCharacter sourceIndex battle
        let battle =
            match command.ActionCommand.Action with
            | Attack | Defend ->
                if source.Healthy && not (Map.containsKey Sleep source.Statuses) then
                    let targetIndexOpt = evalRetarget false targetIndexOpt battle
                    let command = { command with ActionCommand = { command.ActionCommand with TargetIndexOpt = targetIndexOpt }}
                    mapCurrentCommandOpt (constant (Some command)) battle
                else battle
            | Consume consumableType ->
                match Data.Value.Consumables.TryGetValue consumableType with
                | (true, consumable) ->
                    if source.Healthy && not (Map.containsKey Sleep source.Statuses) then
                        let targetIndexOpt = evalRetarget consumable.Revive targetIndexOpt battle
                        let command = { command with ActionCommand = { command.ActionCommand with TargetIndexOpt = targetIndexOpt }}
                        mapCurrentCommandOpt (constant (Some command)) battle
                    else battle
                | (false, _) -> battle
            | Tech techType ->
                match Data.Value.Techs.TryGetValue techType with
                | (true, _) ->
                    if  source.Healthy &&
                        not (Map.containsKey Sleep source.Statuses) &&
                        (not (Map.containsKey Silence source.Statuses) || // NOTE: silence only blocks non-enemy, non-charge techs.
                         source.Enemy && match source.TechChargeOpt with Some (_, chargeAmount, _) -> chargeAmount >= Constants.Battle.ChargeMax | _ -> false) then
                        let targetIndexOpt = evalRetarget false targetIndexOpt battle // TODO: consider affecting wounded, such as for Revive tech.
                        let command = { command with ActionCommand = { command.ActionCommand with TargetIndexOpt = targetIndexOpt }}
                        mapCurrentCommandOpt (constant (Some command)) battle
                    else battle
                | (false, _) -> battle
            | Consequence consequence ->
                match observerIndexOpt with
                | Some observerIndex ->
                    let observerOpt = tryGetCharacter observerIndex battle
                    match (consequence, observerOpt) with
                    | (Spawn _, _) ->
                        let command = { command with ActionCommand = { command.ActionCommand with TargetIndexOpt = targetIndexOpt }}
                        mapCurrentCommandOpt (constant (Some command)) battle
                    | (_, Some observer) when observer.Healthy ->
                        let command = { command with ActionCommand = { command.ActionCommand with TargetIndexOpt = targetIndexOpt }}
                        mapCurrentCommandOpt (constant (Some command)) battle
                    | (_, _) -> battle
                | None -> battle
            | ActionType.Message (_, _) ->
                mapCurrentCommandOpt (constant (Some command)) battle
            | Wound ->
                mapCurrentCommandOpt (constant (Some command)) battle
        let battle = mapActionCommands (constant futureCommands) battle
        update battle

    and private updateNoNextCommand battle =
        let (allySignalsRev, battle) =
            Map.fold (fun (signals : Signal list, battle) allyIndex (ally : Character) ->
                if  ally.ActionTime >= Constants.Battle.ActionTime &&
                    ally.CharacterInputState = NoInput then
                    let battle = mapCharacterInputState (constant RegularMenu) allyIndex battle
                    let playReadySound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ReadySound)
                    (signal playReadySound :: signals, battle)
                else (signals, battle))
                (just battle)
                (getAllies battle)
        let battle =
            Map.fold (fun battle enemyIndex (enemy : Character) ->
                if  enemy.ActionTime >= Constants.Battle.ActionTime &&
                    not (getCharacterAppendedActionCommand enemyIndex battle) then
                    let battle =
                        match enemy.AutoBattleOpt with
                        | Some autoBattle ->
                            let actionCommand =
                                match autoBattle.AutoTechOpt with
                                | Some tech -> ActionCommand.make (Tech tech) enemyIndex (Some autoBattle.AutoTarget) None
                                | None -> ActionCommand.make Attack enemyIndex (Some autoBattle.AutoTarget) None
                            appendActionCommand actionCommand battle
                        | None -> battle    
                    let battle = resetCharacterActionTime enemyIndex battle
                    let battle = resetCharacterInput enemyIndex battle
                    battle
                else battle)
                battle
                (getEnemies battle)
        let battle =
            mapCharacters (fun character ->
                let actionTimeDelta =
                    if character.Ally || battle.BattleSpeed_ = WaitSpeed
                    then Constants.Battle.AllyActionTimeDelta
                    else Constants.Battle.EnemyActionTimeDelta
                let actionTimeDelta =
                    match Map.tryFindKey (function Time _ -> constant true | _ -> constant false) character.Statuses with
                    | Some (Time false) ->
                        let slowScalar =
                            if character.Ally then Constants.Battle.ActionTimeSlowScalar
                            elif character.Boss then Constants.Battle.ActionTimeSlowerScalar
                            else Constants.Battle.ActionTimeSlowestScalar
                        actionTimeDelta * slowScalar
                    | Some (Time true) -> actionTimeDelta * Constants.Battle.ActionTimeHasteScalar
                    | Some _ | None -> actionTimeDelta
                let actionTimeDelta =
                    let anyAlliesInputting = getAlliesHealthy battle |> Map.toValueList |> List.exists (fun ally -> ally.CharacterInputState <> CharacterInputState.NoInput)
                    if anyAlliesInputting then
                        match battle.BattleSpeed_ with
                        | SwiftSpeed -> actionTimeDelta * Constants.Battle.SwiftSpeedScalar
                        | PacedSpeed -> actionTimeDelta * Constants.Battle.PacedSpeedScalar
                        | WaitSpeed -> 0.0f
                    else actionTimeDelta * 1.0f
                let poisoning =
                    let actionTime = character.ActionTime + actionTimeDelta
                    Map.containsKey Poison character.Statuses &&
                    character.ActionTime % 500.0f < 250.0f &&
                    actionTime % 500.0f >= 250.0f
                let character =
                    if character.Healthy && not (Map.containsKey Sleep character.Statuses)
                    then Character.mapActionTime ((+) actionTimeDelta) character
                    else character
                let character =
                    if character.Healthy
                    then Character.burndownStatuses actionTimeDelta character
                    else character
                let character =
                    if character.Healthy && poisoning then
                        let poisonDrainRate =
                            if character.Ally then Constants.Battle.PoisonDrainRateMedium
                            elif character.Boss then Constants.Battle.PoisonDrainRateSlow
                            else Constants.Battle.PoisonDrainRateFast
                        let damage = single character.HitPointsMax * poisonDrainRate |> max 1.0f |> int
                        let alliesHealthy = getAlliesHealthy battle
                        Character.mapHitPoints (fun hp -> (false, max 1 (hp - damage))) false alliesHealthy character
                    else character
                let character =
                    if character.Healthy && Character.readyForAutoBattle character then
                        let jinnInParty = getJinnInParty battle
                        let alliesHealthy = getAlliesHealthy battle
                        let alliesWounded = getAlliesWounded battle
                        let enemiesStanding = getEnemiesStanding battle
                        let enemiesSwooning = getEnemiesSwooning battle
                        Character.autoBattle jinnInParty alliesHealthy alliesWounded enemiesStanding enemiesSwooning character
                    else character
                character)
                battle
        withSignals (List.rev allySignalsRev) battle

    and private updateNoCurrentCommand (battle : Battle) =
        match battle.ActionCommands_ with
        | Queue.Cons (nextCommand, futureCommands) -> updateNextCommand nextCommand futureCommands battle
        | Queue.Nil -> updateNoNextCommand battle

    and private updateRunning (battle : Battle) =
        if battle.MessageOpt_.IsNone then
            match battle.CurrentCommandOpt_ with
            | Some currentCommand -> updateCurrentCommand currentCommand battle
            | None -> updateNoCurrentCommand battle
        else just battle

    and private updateResults startTime outcome (battle : Battle) =
        let localTime = battle.BattleTime_ - startTime
        if localTime = 0L then
            let alliesLevelingUp =
                battle |> getAllies |> Map.toValueList |>
                List.filter (fun ally -> ally.HitPoints > 0) |>
                List.filter (fun ally -> Algorithms.expPointsRemainingForNextLevel ally.ExpPoints <= battle.PrizePool.Exp)
            let textA =
                match alliesLevelingUp with
                | _ :: _ -> "Level up for " + (alliesLevelingUp |> List.map (fun c -> c.Name) |> String.join ", ") + "!^"
                | [] -> "Enemies defeated!^"
            let textB =
                alliesLevelingUp |>
                List.choose (fun ally ->
                    let techs = Algorithms.expPointsToTechs3 ally.ExpPoints battle.PrizePool_.Exp ally.ArchetypeType
                    if Set.notEmpty techs then Some (ally, techs) else None) |>
                List.map (fun (ally, techs) ->
                    let text = techs |> Set.toList |> List.map scstring |> String.join ", "
                    ally.Name + " learned " + text + "!") |>
                function
                | _ :: _ as texts -> String.join "\n" texts + "^"
                | [] -> ""
            let textC = "Gained " + string battle.PrizePool_.Exp + " Exp!\nGained " + string battle.PrizePool_.Gold + " Gold!"
            let textD =
                match battle.PrizePool_.Items with
                | _ :: _ as items -> "^Found " + (items |> List.map (fun i -> ItemType.getName i) |> String.join ", ") + "!"
                | [] -> ""
            let text = textA + textB + textC + textD
            let dialog = Dialog.make DialogThick text
            let battle = mapDialogOpt (constant (Some dialog)) battle
            let (sigs, battle) =
                if outcome then
                    let battle = mapAllies (fun ally -> if ally.Healthy then Character.mapExpPoints ((+) battle.PrizePool_.Exp) ally else ally) battle
                    let battle =
                        mapAllies (fun ally ->
                            if List.exists (fun (ally' : Character) -> ally.CharacterIndex = ally'.CharacterIndex) alliesLevelingUp
                            then Character.restore ally
                            else ally)
                            battle
                    let battle = mapInventory (fun inv -> { inv with Gold = inv.Gold + battle.PrizePool_.Gold }) battle
                    let battle = mapInventory (Inventory.tryAddItems battle.PrizePool_.Items >> snd) battle
                    if List.notEmpty alliesLevelingUp
                    then withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.GrowthSound)) battle
                    else just battle
                else just battle
            (signal (FadeOutSong 360L) :: sigs, battle)
        else
            match battle.DialogOpt_ with
            | None -> just (mapBattleState (constant (BattleQuitting (battle.BattleTime_, outcome, battle.PrizePool_.Consequents))) battle)
            | Some _ -> just battle

    and private updateCease startTime (battle : Battle) =
        ignore<int64> startTime
        just battle

    and update (battle : Battle) : Signal list * Battle =

        // update message
        let battle =
            mapMessageOpt (function
                | Some (startTime, lifeTime, message) when battle.BattleTime_ < startTime + lifeTime -> Some (startTime, lifeTime, Dialog.update id battle.BattleTime_ message)
                | Some _ | None -> None)
                battle

        // update dialog
        let battle =
            mapDialogOpt (function
                | Some dialog -> Some (Dialog.update id battle.BattleTime_ dialog)
                | None -> None)
                battle

        // update battle state
        let (signals, battle) =
            match battle.BattleState_ with
            | BattleReady startTime -> updateReady startTime battle
            | BattleRunning -> updateRunning battle
            | BattleResult (startTime, outcome) -> updateResults startTime outcome battle
            | BattleQuitting (startTime, _, _) -> updateCease startTime battle
            | BattleQuit -> just battle

        // fin
        (signals, battle)

    let updateBattleTime field =
        let field = { field with BattleTime_ = inc field.BattleTime_ }
        just field

    let makeFromParty inventory (prizePool : PrizePool) (party : Party) battleSpeed battleData =
        let enemies = randomizeEnemies party.Length (battleSpeed = WaitSpeed) battleData.BattleEnemies
        let characters = party @ enemies |> Map.ofListBy (fun (character : Character) -> (character.CharacterIndex, character))
        let prizePool = { prizePool with Gold = List.fold (fun gold (enemy : Character) -> gold + enemy.GoldPrize) prizePool.Gold enemies }
        let prizePool = { prizePool with Exp = List.fold (fun exp (enemy : Character) -> exp + enemy.ExpPrize) prizePool.Exp enemies }
        let prizePool = { prizePool with Items = List.fold (fun items (enemy : Character) -> match enemy.ItemPrizeOpt with Some item -> item :: items | None -> items) prizePool.Items enemies }
        let tileMap = battleData.BattleTileMap
        let tileIndexOffset = battleData.BattleTileIndexOffset
        let tileIndexOffsetRange = battleData.BattleTileIndexOffsetRange
        let battle =
            { BattleTime_ = 0L
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
              MessageOpt_ = None
              DialogOpt_ = None
              BattleState_ = BattleReady 1L }
        battle

    let empty =
        match Map.tryFind EmptyBattle Data.Value.Battles with
        | Some battle ->
            { BattleTime_ = 0L
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
              MessageOpt_ = None
              DialogOpt_ = None
              BattleState_ = BattleQuit }
        | None -> failwith "Expected data for DebugBattle to be available."

type Battle = Battle.Battle