namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniBattle =

    type [<NoComparison>] Hop =
        { HopStart : Vector2
          HopStop : Vector2 }

    type BattleMessage =
        | RegularItemSelect of CharacterIndex * string
        | RegularItemCancel of CharacterIndex
        | SpecialItemSelect of CharacterIndex * string
        | SpecialItemCancel of CharacterIndex
        | ItemItemSelect of CharacterIndex * string
        | ItemItemCancel of CharacterIndex
        | ReticlesSelect of CharacterIndex * CharacterIndex
        | ReticlesCancel of CharacterIndex
        | ReadyCharacters
        | PoiseCharacters
        | CelebrateCharacters of bool
        | AttackCharacter1 of CharacterIndex
        | AttackCharacter2 of CharacterIndex * CharacterIndex
        | SpecialCharacter1 of CharacterIndex * CharacterIndex * SpecialType
        | SpecialCharacter2 of CharacterIndex * CharacterIndex * SpecialType
        | SpecialCharacter3 of CharacterIndex * CharacterIndex * SpecialType
        | SpecialCharacter4 of CharacterIndex * CharacterIndex * SpecialType
        | SpecialCharacter5 of CharacterIndex * CharacterIndex * SpecialType
        | SpecialCharacterAmbient of CharacterIndex * CharacterIndex * SpecialType
        | ConsumeCharacter1 of ConsumableType * CharacterIndex
        | ConsumeCharacter2 of ConsumableType * CharacterIndex
        | ChargeCharacter of CharacterIndex
        | PoiseCharacter of CharacterIndex
        | WoundCharacter of CharacterIndex
        | ResetCharacter of CharacterIndex
        | DestroyCharacter of CharacterIndex
        | Tick

    type [<NoComparison>] BattleCommand =
        | FadeSong
        | PlaySound of int64 * single * AssetTag<Audio>
        | DisplayCancel of CharacterIndex
        | DisplayHitPointsChange of CharacterIndex * int
        | DisplayBolt of CharacterIndex
        | DisplayHop of Hop
        | InitializeBattle
        | FinalizeBattle

    type Screen with

        member this.GetBattleModel = this.GetModel<BattleModel>
        member this.SetBattleModel = this.SetModel<BattleModel>
        member this.BattleModel = this.Model<BattleModel> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<BattleModel, BattleMessage, BattleCommand>
            (let allies =
                [CharacterModel.make
                    { PartyIndex = 0; CharacterType = Ally Jinn; ActionTime = 600; ExpPoints = 0; HitPoints = 10; SpecialPoints = 10; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [HeadSlash; Cyclone]; Statuses = Set.empty; WeaponOpt = Some "WoodenSword"; ArmorOpt = Some "LeatherVest"; Accessories = []; AutoBattleOpt = None }
                    { TimeStart = 0L; AnimationSheet = Assets.JinnAnimationSheet; AnimationCycle = ReadyCycle; Direction = Rightward }
                    NoInput
                    (Math.makeBounds (v2 -224.0f -168.0f) (v2 160.0f 160.0f))
                 CharacterModel.make
                    { PartyIndex = 1; CharacterType = Ally Glenn; ActionTime = 420; ExpPoints = 0; HitPoints = 10; SpecialPoints = 10; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [HeadSlash; Tremor]; Statuses = Set.empty; WeaponOpt = Some "OakRod"; ArmorOpt = Some "LeatherRobe"; Accessories = []; AutoBattleOpt = None }
                    { TimeStart = 0L; AnimationSheet = Assets.GlennAnimationSheet; AnimationCycle = ReadyCycle; Direction = Leftward }
                    NoInput
                    (Math.makeBounds (v2 224.0f 64.0f) (v2 160.0f 160.0f))]
             let enemies =
                [CharacterModel.make
                    { PartyIndex = 0; CharacterType = Enemy Goblin; ActionTime = 99; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [Bolt]; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Accessories = []; AutoBattleOpt = None; }
                    { TimeStart = 0L; AnimationSheet = Assets.GoblinAnimationSheet; AnimationCycle = ReadyCycle; Direction = Leftward }
                    NoInput
                    (Math.makeBounds (v2 0.0f 0.0f) (v2 160.0f 160.0f))
                 CharacterModel.make
                    { PartyIndex = 1; CharacterType = Enemy Goblin; ActionTime = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [Bolt]; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Accessories = []; AutoBattleOpt = None; }
                    { TimeStart = 0L; AnimationSheet = Assets.GoblinAnimationSheet; AnimationCycle = ReadyCycle; Direction = Leftward }
                    NoInput
                    (Math.makeBounds (v2 176.0f -192.0f) (v2 160.0f 160.0f))]
             let characters =
                Map.ofList
                    (List.mapi (fun i ally -> (AllyIndex i, ally)) allies @
                     List.mapi (fun i enemy -> (EnemyIndex i, enemy)) enemies)
             let inventory = { Items = Map.ofList [(Consumable GreenHerb, 2); (Consumable RedHerb, 2)] }
             let model = BattleModel.make (BattleReady 0L) characters None Queue.empty inventory 100
             model)

        static let tickAttack sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            match targetIndexOpt with
            | Some targetIndex ->
                let target = BattleModel.getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.IsHealthy
                    then withMsg model (AttackCharacter1 sourceIndex)
                    else
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | 15L -> withMsg model (AttackCharacter2 (sourceIndex, targetIndex))
                | _ when CharacterModel.getAnimationFinished time target ->
                    let target = BattleModel.getCharacter targetIndex model
                    if target.IsHealthy then
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                    else
                        let woundCommand = CurrentCommand.make time (ActionCommand.make Wound sourceIndex (Some targetIndex))
                        let model = BattleModel.updateCurrentCommandOpt (constant (Some woundCommand)) model
                        withMsg model (PoiseCharacter sourceIndex)
                | _ -> just model
            | None ->
                let model = BattleModel.updateCurrentCommandOpt (constant None) model
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickSpecial specialType sourceIndex (targetIndexOpt : CharacterIndex option) (_ : int64) timeLocal model =
            match targetIndexOpt with
            | Some targetIndex ->
                let (model, msgs) =
                    match timeLocal with
                    | 0L -> (model, [SpecialCharacter1 (sourceIndex, targetIndex, specialType)]) // charge or hop forward
                    | 30L -> (model, [SpecialCharacter2 (sourceIndex, targetIndex, specialType)]) // casting
                    | 80L -> (model, [SpecialCharacter3 (sourceIndex, targetIndex, specialType)]) // outcome
                    | 90L -> (model, [SpecialCharacter4 (sourceIndex, targetIndex, specialType)]) // hop back
                    | 110L -> (model, [SpecialCharacter5 (sourceIndex, targetIndex, specialType)]) // poise
                    | _ -> (model, [])
                let (model, msgs) = (model, msgs @ [SpecialCharacterAmbient (sourceIndex, targetIndex, specialType)])
                withMsgs model msgs
            | None ->
                let model = BattleModel.updateCurrentCommandOpt (constant None) model
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickConsume consumable sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            match targetIndexOpt with
            | Some targetIndex ->
                let target = BattleModel.getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.IsHealthy
                    then withMsg model (ConsumeCharacter1 (consumable, sourceIndex))
                    else
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | 30L -> withMsg model (ConsumeCharacter2 (consumable, targetIndex))
                | _ when CharacterModel.getAnimationFinished time target ->
                    let model = BattleModel.updateCurrentCommandOpt (constant None) model
                    withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                | _ -> just model
            | None ->
                let model = BattleModel.updateCurrentCommandOpt (constant None) model
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickWound targetIndex time model =
            let character = BattleModel.getCharacter targetIndex model
            if character.IsAlly then
                match character.AnimationCycle with
                | DamageCycle ->
                    if CharacterModel.getAnimationFinished time character then
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsg model (WoundCharacter targetIndex)
                    else just model
                | _ -> failwithumf ()
            else
                match character.AnimationCycle with
                | DamageCycle ->
                    if CharacterModel.getAnimationFinished time character then
                        let woundCharacter = WoundCharacter targetIndex
                        let playDeathSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.DeathSound)
                        withSigs model [Message woundCharacter; Command playDeathSound]
                    else just model
                | WoundCycle ->
                    if CharacterModel.getAnimationFinished time character then
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsg model (DestroyCharacter targetIndex)
                    else just model
                | _ -> failwithumf ()

        static let tickReady time timeStart model =
            let timeLocal = time - timeStart
            match timeLocal with
            | 0L -> withMsg model ReadyCharacters
            | 30L ->
                let model = BattleModel.updateBattleState (constant BattleRunning) model
                withMsg model PoiseCharacters
            | _ -> just model

        static let rec tickCurrentCommand time currentCommand model =
            let timeLocal = time - currentCommand.TimeStart
            match currentCommand.ActionCommand.Action with
            | Attack ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickAttack source targetOpt time timeLocal model
            | Consume consumable ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickConsume consumable source targetOpt time timeLocal model
            | Special specialType ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickSpecial specialType source targetOpt time timeLocal model
            | Wound ->
                match currentCommand.ActionCommand.TargetOpt with
                | Some target ->
                    let (model, signal) = tickWound target time model
                    match model.CurrentCommandOpt with
                    | Some _ ->
                        // just keep ticking wound...
                        withSig model signal
                    | None ->
                        let (model, signal2) =
                            let allies = BattleModel.getAllies model
                            let enemies = BattleModel.getEnemies model
                            if Seq.forall (fun (character : CharacterModel) -> character.IsWounded) allies then
                                let model = BattleModel.updateBattleState (constant (BattleCease (false, time))) model
                                tick time model // tick for frame 0
                            elif Seq.forall (fun (character : CharacterModel) -> character.IsWounded) enemies then
                                let model = BattleModel.updateBattleState (constant (BattleCease (true, time))) model
                                tick time model // tick for frame 0
                            else just model
                        withSig model (signal + signal2)
                | None -> just model
        
        and tickNextCommand time nextCommand futureCommands (model : BattleModel) =
            let command = CurrentCommand.make time nextCommand
            let model = BattleModel.updateCurrentCommandOpt (constant (Some command)) model
            let model = BattleModel.updateActionCommands (constant futureCommands) model
            tick time model // tick for frame 0

        and tickNoNextCommand time (model : BattleModel) =
            let (allySignalsRev, model) =
                List.fold (fun (signals, model) (ally : CharacterModel) ->
                    if ally.ActionTime = Constants.Battle.ActionTime then
                        let model =
                            BattleModel.updateCharacter
                                (fun character ->
                                    let character = CharacterModel.updateInputState (constant RegularMenu) character
                                    let character = CharacterModel.animate time (PoiseCycle Poising) character
                                    character)
                                ally.CharacterIndex
                                model
                        let playActionTimeSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.AffirmSound)
                        (Command playActionTimeSound :: signals, model)
                    else (signals, model))
                    ([], model)
                    (BattleModel.getAllies model)
            let (enemySignalsRev, model) =
                List.fold (fun (signals, model) (enemy : CharacterModel) ->
                    if enemy.ActionTime = Constants.Battle.ActionTime then
                        let enemyIndex = enemy.CharacterIndex
                        match enemy.AutoBattleOpt with
                        | Some autoBattle ->
                            let actionCommand =
                                match autoBattle.AutoSpecialOpt with
                                | Some special -> { Action = Special special; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                                | None -> { Action = Attack; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                            let model = BattleModel.conjActionCommand actionCommand model
                            (Message (ResetCharacter enemyIndex) :: signals, model)
                        | None -> (Message (ResetCharacter enemyIndex) :: signals, model)
                    else (signals, model))
                    ([], model)
                    (BattleModel.getEnemies model)
            let model =
                BattleModel.updateCharacters (fun character ->
                    let character =
                        CharacterModel.updateActionTime ((+) Constants.Battle.ActionTimeInc) character
                    let character =
                        if CharacterModel.isReadyForAutoBattle character then
                            let targetIndex = BattleModel.getAllyIndexRandom model
                            let target = BattleModel.getCharacter targetIndex model
                            CharacterModel.autoBattle character target
                        else character
                    character)
                    model
            withSigs model (List.rev (allySignalsRev @ enemySignalsRev))

        and tickNoCurrentCommand time (model : BattleModel) =
            match model.ActionCommands with
            | Queue.Cons (nextCommand, futureCommands) -> tickNextCommand time nextCommand futureCommands model
            | Queue.Nil -> tickNoNextCommand time model

        and tickRunning time (model : BattleModel) =
            match model.CurrentCommandOpt with
            | Some currentCommand -> tickCurrentCommand time currentCommand model
            | None -> tickNoCurrentCommand time model

        and tickCease time timeStart outcome model =
            let timeLocal = time - timeStart
            match timeLocal with
            | 0L -> withMsg model (CelebrateCharacters outcome)
            | _ -> just model

        and tick time model =
            let (model, sigs) =
                match model.BattleState with
                | BattleReady timeStart -> tickReady time timeStart model
                | BattleRunning -> tickRunning time model
                | BattleCease (outcome, timeStart) -> tickCease time timeStart outcome model
            (model, sigs)

        override this.Bindings (_, battle, _) =
            [battle.OutgoingStartEvent => cmd FadeSong
             battle.SelectEvent => cmd InitializeBattle
             battle.DeselectEvent => cmd FinalizeBattle
             battle.UpdateEvent => msg Tick]

        override this.Message (model, message, screen, world) =

            match message with
            | RegularItemSelect (characterIndex, item) ->
                let model =
                    match item with
                    | "Attack" ->
                        BattleModel.updateCharacter
                            (CharacterModel.updateInputState (constant (AimReticles (item, EnemyAim))))
                            characterIndex
                            model
                    | "Special" ->
                        BattleModel.updateCharacter
                            (CharacterModel.updateInputState (constant SpecialMenu))
                            characterIndex
                            model
                    | "Item" ->
                        BattleModel.updateCharacter
                            (CharacterModel.updateInputState (constant ItemMenu))
                            characterIndex
                            model
                    | "Defend" ->
                        BattleModel.updateCharacter
                            (fun character ->
                                let time = World.getTickTime world
                                let character = CharacterModel.updateActionTime (constant 0) character
                                let character = CharacterModel.updateInputState (constant NoInput) character
                                let character = CharacterModel.defend character
                                let character = CharacterModel.animate time (PoiseCycle Defending) character
                                character)
                            characterIndex
                            model
                    | _ -> failwithumf ()
                just model
            
            | RegularItemCancel characterIndex ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant RegularMenu))
                        characterIndex
                        model
                just model
            
            | SpecialItemSelect (characterIndex, item) ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant (AimReticles (item, EnemyAim))))
                        characterIndex
                        model
                just model
            
            | SpecialItemCancel characterIndex ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant RegularMenu))
                        characterIndex
                        model
                just model
            
            | ItemItemSelect (characterIndex, item) ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant (AimReticles (item, AllyAim true))))
                        characterIndex
                        model
                just model

            | ItemItemCancel characterIndex ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant RegularMenu))
                        characterIndex
                        model
                just model

            | ReticlesSelect (targetIndex, allyIndex) ->
                match model.BattleState with
                | BattleRunning ->
                    let ally = BattleModel.getCharacter allyIndex model
                    match ally.InputState with
                    | AimReticles (item, _) ->
                        let command =
                            match item with
                            | "GreenHerb" -> ActionCommand.make (Consume GreenHerb) allyIndex (Some targetIndex)
                            | "RedHerb" -> ActionCommand.make (Consume RedHerb) allyIndex (Some targetIndex)
                            | "HeadSlash" -> ActionCommand.make (Special HeadSlash) allyIndex (Some targetIndex)
                            | "Cyclone" -> ActionCommand.make (Special Cyclone) allyIndex (Some targetIndex)
                            | "Bolt" -> ActionCommand.make (Special Bolt) allyIndex (Some targetIndex)
                            | "Tremor" -> ActionCommand.make (Special Tremor) allyIndex (Some targetIndex)
                            | _ -> ActionCommand.make Attack allyIndex (Some targetIndex)
                        let model = BattleModel.conjActionCommand command model
                        withMsg model (ResetCharacter allyIndex)
                    | _ -> just model
                | _ -> just model

            | ReticlesCancel characterIndex ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant RegularMenu))
                        characterIndex
                        model
                just model

            | ReadyCharacters ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacters
                        (CharacterModel.animate time ReadyCycle)
                        model
                just model

            | PoiseCharacters ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacters
                        (fun character ->
                            let poiseType = CharacterModel.getPoiseType character
                            let character = CharacterModel.animate time (PoiseCycle poiseType) character
                            character)
                        model
                just model

            | CelebrateCharacters outcome ->
                let time = World.getTickTime world
                let model =
                    if outcome
                    then BattleModel.updateAllies (CharacterModel.animate time CelebrateCycle) model
                    else BattleModel.updateEnemies (CharacterModel.animate time CelebrateCycle) model
                just model

            | AttackCharacter1 sourceIndex ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.animate time AttackCycle) sourceIndex model
                let playHitSoundDelay = 15L
                let playHitSound = PlaySound (playHitSoundDelay, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                withCmd model playHitSound

            | AttackCharacter2 (sourceIndex, targetIndex) ->
                let time = World.getTickTime world
                let rom = screen.Parent.GetModel<Rom> world
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                let damage = CharacterModel.getAttackResult rom source target
                let model = BattleModel.updateCharacter (CharacterModel.updateHitPoints rom (fun hitPoints -> (hitPoints - damage, false))) targetIndex model
                let (model, sigs) =
                    if target.HitPoints <= 0
                    then (model, [Message (ResetCharacter targetIndex)]) // wounded
                    else (BattleModel.updateCharacter (CharacterModel.animate time DamageCycle) targetIndex model, []) // just damaged
                let sigs = Command (DisplayHitPointsChange (targetIndex, -damage)) :: sigs
                withSigs model sigs
            
            | SpecialCharacter1 (sourceIndex, targetIndex, specialType) ->
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                let hopOpt =
                    match specialType with
                    | Cyclone -> Some { HopStart = source.Center; HopStop = target.Bottom }
                    | HeadSlash | Bolt | Tremor -> None
                match hopOpt with
                | None ->
                    if target.IsHealthy
                    then withMsg model (ChargeCharacter sourceIndex)
                    else
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | Some hop ->
                    withCmd model (DisplayHop hop)

            | SpecialCharacter2 (sourceIndex, targetIndex, specialType) ->
                match specialType with
                | HeadSlash ->
                    let time = World.getTickTime world
                    let model = BattleModel.updateCharacter (CharacterModel.animate time AttackCycle) sourceIndex model
                    let playHitSoundDelay = 10L
                    let playHitSound = PlaySound (playHitSoundDelay, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                    withCmd model playHitSound
                | Cyclone ->
                    let time = World.getTickTime world
                    let model = BattleModel.updateCharacter (CharacterModel.animate time WhirlCycle) sourceIndex model
                    let playHitSounds =
                        [PlaySound (20L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                         PlaySound (40L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                         PlaySound (60L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)]
                    withCmds model playHitSounds
                | Bolt ->
                    let time = World.getTickTime world
                    let model = BattleModel.updateCharacter (CharacterModel.animate time Cast2Cycle) sourceIndex model
                    withCmd model (DisplayBolt targetIndex)
                | Tremor ->
                    let time = World.getTickTime world
                    let model = BattleModel.updateCharacter (CharacterModel.animate time BuryCycle) sourceIndex model
                    withCmd model (DisplayBolt targetIndex)

            | SpecialCharacter3 (sourceIndex, targetIndex, specialType) ->
                let time = World.getTickTime world
                let rom = screen.Parent.GetModel<Rom> world
                match Map.tryFind specialType rom.Specials with
                | Some specialData ->
                    let source = BattleModel.getCharacter sourceIndex model
                    let target = BattleModel.getCharacter targetIndex model
                    let (cancelled, hitPointsChange) = CharacterModel.evaluateSpecialMove rom specialData source target
                    let model = BattleModel.updateCharacter (CharacterModel.updateSpecialPoints rom ((+) -specialData.SpecialCost)) sourceIndex model
                    let model = BattleModel.updateCharacter (CharacterModel.updateHitPoints rom (fun hitPoints -> (hitPoints + hitPointsChange, cancelled))) targetIndex model
                    let model =
                        if hitPointsChange < 0 && target.IsHealthy
                        then BattleModel.updateCharacter (CharacterModel.animate time DamageCycle) targetIndex model
                        else model
                    let sigs = if target.IsWounded then [Message (ResetCharacter targetIndex)] else []
                    let sigs = if cancelled then Command (DisplayCancel targetIndex) :: sigs else sigs
                    let sigs = if hitPointsChange <> 0 then Command (DisplayHitPointsChange (targetIndex, hitPointsChange)) :: sigs else sigs
                    withSigs model sigs
                | None -> just model

            | SpecialCharacter4 (sourceIndex, targetIndex, specialType) ->
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                let hopOpt =
                    match specialType with
                    | Cyclone -> Some { HopStart = target.Bottom; HopStop = source.CenterOriginal }
                    | HeadSlash | Bolt | Tremor -> None
                match hopOpt with
                | None -> just model
                | Some hop -> withCmd model (DisplayHop hop)

            | SpecialCharacter5 (sourceIndex, targetIndex, _) ->
                let time = World.getTickTime world
                let target = BattleModel.getCharacter targetIndex model
                if target.IsHealthy then
                    let model = BattleModel.updateCurrentCommandOpt (constant None) model
                    withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                else
                    let woundCommand = CurrentCommand.make time (ActionCommand.make Wound sourceIndex (Some targetIndex))
                    let model = BattleModel.updateCurrentCommandOpt (constant (Some woundCommand)) model
                    withMsgs model [PoiseCharacter sourceIndex]
                    
            | SpecialCharacterAmbient (sourceIndex, _, _) ->
                if Simulants.BattleHop.GetExists world then
                    let model =
                        let tags = Simulants.BattleHop.GetEffectTags world
                        match Map.tryFind "Hop" tags with
                        | Some tag -> BattleModel.updateCharacter (CharacterModel.updateCenter (constant tag.Position)) sourceIndex model
                        | None -> model
                    just model
                else just model

            | ConsumeCharacter1 (consumable, sourceIndex) ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.animate time CastCycle) sourceIndex model
                let item = Consumable consumable
                let model = BattleModel.updateInventory (Inventory.removeItem item) model
                just model

            | ConsumeCharacter2 (consumable, targetIndex) ->
                let time = World.getTickTime world
                let rom = screen.Parent.GetModel<Rom> world
                let healing =
                    match consumable with
                    | GreenHerb -> 50 // TODO: pull from rom data
                    | RedHerb -> 500 // TODO: pull from rom data
                let model = BattleModel.updateCharacter (CharacterModel.updateHitPoints rom (fun hitPoints -> (hitPoints + healing, false))) targetIndex model
                let model = BattleModel.updateCharacter (CharacterModel.animate time SpinCycle) targetIndex model
                let displayHitPointsChange = DisplayHitPointsChange (targetIndex, healing)
                let playHealSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.HealSound)
                withCmds model [displayHitPointsChange; playHealSound]

            | ChargeCharacter sourceIndex ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.animate time (PoiseCycle Charging)) sourceIndex model
                just model

            | PoiseCharacter characterIndex ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacter
                        (fun character ->
                            let poiseType = CharacterModel.getPoiseType character
                            let character = CharacterModel.animate time (PoiseCycle poiseType) character
                            character)
                        characterIndex
                        model
                just model

            | WoundCharacter characterIndex ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacter
                        (fun character ->
                            let character =
                                if character.IsAlly
                                then CharacterModel.updateInputState (constant NoInput) character
                                else character
                            let character = CharacterModel.animate time WoundCycle character
                            character)
                        characterIndex
                        model
                just model

            | ResetCharacter characterIndex ->
                let character = BattleModel.getCharacter characterIndex model
                let model = BattleModel.updateCharacter (CharacterModel.updateActionTime (constant 0)) characterIndex model
                let model =
                    if character.IsAlly
                    then BattleModel.updateCharacter (CharacterModel.updateInputState (constant NoInput)) characterIndex model
                    else BattleModel.updateCharacter (CharacterModel.updateAutoBattleOpt (constant None)) characterIndex model
                just model

            | DestroyCharacter characterIndex ->
                let character = BattleModel.getCharacter characterIndex model
                let model = if character.IsEnemy then BattleModel.removeCharacter characterIndex model else model
                just model

            | Tick ->
                if World.isTicking world
                then tick (World.getTickTime world) model
                else just model

        override this.Command (model, command, battle, world) =

            match command with
            | FadeSong ->
                let world = World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
                just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

            | DisplayCancel targetIndex ->
                match BattleModel.tryGetCharacter targetIndex model with
                | Some target ->
                    let effect = Effects.makeCancelEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset3 world
                    let world = entity.SetDepth (Constants.Battle.GuiEffectDepth - 1.0f) world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayHitPointsChange (targetIndex, delta) ->
                match BattleModel.tryGetCharacter targetIndex model with
                | Some target ->
                    let effect = Effects.makeHitPointsChangeEffect delta
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset2 world
                    let world = entity.SetDepth Constants.Battle.GuiEffectDepth world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayBolt targetIndex ->
                match BattleModel.tryGetCharacter targetIndex model with
                | Some target ->
                    let effect = Effects.makeBoltEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let world = entity.SetEffect effect world
                    let world = entity.SetSize (v2 256.0f 1024.0f) world
                    let world = entity.SetBottom target.Bottom world
                    let world = entity.SetDepth Constants.Battle.EffectDepth world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayHop hop ->
                let effect = Effects.makeHopEffect hop.HopStart hop.HopStop
                let (entity, world) = World.createEntity<EffectDispatcher> (Some Simulants.BattleHop.Name) DefaultOverlay Simulants.BattleScene world
                let world = entity.SetEffect effect world
                let world = entity.SetEffectOffset v2Zero world
                let world = entity.SetSelfDestruct true world
                just world

            | InitializeBattle ->
                let world = World.hintRenderPackageUse Assets.BattlePackageName world
                let world = World.hintAudioPackageUse Assets.BattlePackageName world
                let world = World.playSong 0 Constants.Audio.DefaultSongVolume Assets.BattleSong world
                let model = BattleModel.updateBattleState (constant (BattleReady (World.getTickTime world))) model
                let world = battle.SetBattleModel model world
                just world

            | FinalizeBattle ->
                let world = World.hintRenderPackageDisuse Assets.BattlePackageName world
                just (World.hintAudioPackageDisuse Assets.BattlePackageName world)

        member private this.SceneContent (model : Lens<BattleModel, World>, _ : Screen, _ : World) =

            let background = Simulants.BattleScene / "Background"
            Content.layer Simulants.BattleScene.Name []

                [Content.label background.Name
                    [background.Position == v2 -480.0f -512.0f
                     background.Size == v2 1024.0f 1024.0f
                     background.Depth == Constants.Battle.BackgroundDepth
                     background.LabelImage == asset "Battle" "Background"]

                 Content.entitiesIndexedBy
                    (model --> fun model -> BattleModel.getAllies model)
                    (fun model -> model.PartyIndex)
                    (fun index model _ -> Content.entity<CharacterDispatcher> ("Ally+" + scstring index) [Entity.CharacterModel <== model])

                 Content.entitiesIndexedBy
                    (model --> fun model -> BattleModel.getEnemies model)
                    (fun model -> model.PartyIndex)
                    (fun index model _ -> Content.entity<CharacterDispatcher> ("Enemy+" + scstring index) [Entity.CharacterModel <== model])]

        member private this.InputContent (model : Lens<BattleModel, World>, screen : Screen, _ : World) =

            Content.layers (model --> fun model -> BattleModel.getAllies model) $ fun index ally _ ->

                let allyIndex = AllyIndex index
                let input = screen / ("Input" + "+" + scstring index)
                Content.layer input.Name []

                    [Content.fillBar "HealthBar" 
                        [Entity.Size == v2 64.0f 8.0f
                         Entity.Center <== ally --> fun ally -> ally.BottomOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.HitPoints > 0
                         Entity.Fill <== ally ->> fun ally world ->
                            let rom = screen.Parent.GetModel<Rom> world
                            single ally.HitPoints / single (ally.HitPointsMax rom)]

                     Content.entity<RingMenuDispatcher> "RegularMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = RegularMenu
                         Entity.Enabled <== model --> fun model ->
                            let allies = BattleModel.getAllies model
                            let alliesPastRegularMenu =
                                List.notExists (fun (ally : CharacterModel) ->
                                    match ally.InputState with NoInput | RegularMenu -> false | _ -> true)
                                    allies
                            alliesPastRegularMenu
                         Entity.RingMenuModel == { Items = [(0, (true, "Attack")); (1, (true, "Defend")); (2, (true, "Item")); (3, (true, "Special"))]; ItemCancelOpt = None }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (RegularItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (RegularItemCancel allyIndex)]

                     Content.entity<RingMenuDispatcher> "SpecialMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = SpecialMenu
                         Entity.RingMenuModel <== ally ->> fun ally world ->
                            let rom = screen.Parent.GetModel<Rom> world
                            let specials = List.ofSeq ally.Specials
                            let specials =
                                List.map (fun special ->
                                    let specialTag = getTag special
                                    let specialUsable =
                                        match Map.tryFind special rom.Specials with
                                        | Some specialData -> specialData.SpecialCost <= ally.SpecialPoints
                                        | None -> false
                                    let specialName = scstring special
                                    (specialTag, (specialUsable, specialName)))
                                    specials
                            { Items = specials; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (SpecialItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (SpecialItemCancel allyIndex)]

                     Content.entity<RingMenuDispatcher> "ItemMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = ItemMenu
                         Entity.RingMenuModel <== model --> fun model ->
                            let consumables = Inventory.getConsumables model.Inventory
                            let consumables = Map.toKeyList consumables
                            let consumables = List.map (fun special -> (getTag special, (true, scstring special))) consumables
                            { Items = consumables; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (ItemItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (ItemItemCancel allyIndex)]
                     Content.entity<ReticlesDispatcher> "Reticles"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> match ally.InputState with AimReticles _ -> true | _ -> false
                         Entity.ReticlesModel <== model --> fun model -> { BattleModel = model; AimType = (BattleModel.getCharacter allyIndex model).InputState.AimType }
                         Entity.TargetSelectEvent ==|> fun evt -> msg (ReticlesSelect (evt.Data, allyIndex))
                         Entity.CancelEvent ==> msg (ReticlesCancel allyIndex)]]

        override this.Content (model, screen, world) =
            [this.SceneContent (model, screen, world)
             this.InputContent (model, screen, world)]