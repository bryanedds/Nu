namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniBattle =

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
        | AttackCharacter of CharacterIndex
        | DamageCharacter of CharacterIndex * CharacterIndex * SpecialType option
        | ChargeCharacter of CharacterIndex
        | PoiseCharacter of CharacterIndex
        | WoundCharacter of CharacterIndex
        | ResetCharacter of CharacterIndex
        | DestroyCharacter of CharacterIndex
        | GiveConsumable of ConsumableType * CharacterIndex
        | TakeConsumable of ConsumableType * CharacterIndex
        | Tick

    type BattleCommand =
        | FadeSong
        | PlaySound of int64 * single * AssetTag<Audio>
        | DisplayCancel of CharacterIndex
        | DisplayHitPointsChange of CharacterIndex * int
        | InitializeBattle
        | FinalizeBattle

    type Screen with

        member this.GetBattleModel = this.GetModel<BattleModel>
        member this.SetBattleModel = this.SetModel<BattleModel>
        member this.BattleModel = this.Model<BattleModel> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<BattleModel, BattleMessage, BattleCommand>
            (let allies =
                [{ CharacterState = { CharacterType = Ally Jinn; PartyIndex = 0; ExpPoints = 0; HitPoints = 10; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [HeadSlash; Bolt]; Statuses = Set.empty; WeaponOpt = Some "WoodenSword"; ArmorOpt = Some "LeatherVest"; Accessories = []; AutoBattleOpt = None }
                   AnimationState = { TimeStart = 0L; AnimationSheet = Assets.JinnAnimationSheet; AnimationCycle = ReadyCycle; Direction = Rightward; Stutter = 10 }
                   ActionTime = 600
                   InputState = NoInput
                   Position = v2 -224.0f -168.0f
                   Size = v2 160.0f 160.0f }
                 { CharacterState = { CharacterType = Ally Glenn; PartyIndex = 1; ExpPoints = 0; HitPoints = 10; SpecialPoints = 0; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [HeadSlash; Bolt]; Statuses = Set.empty; WeaponOpt = Some "OakRod"; ArmorOpt = Some "LeatherRobe"; Accessories = []; AutoBattleOpt = None }
                   AnimationState = { TimeStart = 0L; AnimationSheet = Assets.GlennAnimationSheet; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   ActionTime = 420
                   InputState = NoInput
                   Position = v2 224.0f 64.0f
                   Size = v2 160.0f 160.0f }]
             let enemies =
                [{ CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [HeadSlash]; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Accessories = []; AutoBattleOpt = None; }
                   AnimationState = { TimeStart = 0L; AnimationSheet = Assets.GoblinAnimationSheet; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   ActionTime = 99
                   InputState = NoInput
                   Position = v2 0.0f 0.0f
                   Size = v2 160.0f 160.0f }
                 { CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 1; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [HeadSlash]; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Accessories = []; AutoBattleOpt = None; }
                   AnimationState = { TimeStart = 0L; AnimationSheet = Assets.GoblinAnimationSheet; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   ActionTime = 0
                   InputState = NoInput
                   Position = v2 176.0f -192.0f
                   Size = v2 160.0f 160.0f }]
             let characters =
                Map.ofList
                    (List.mapi (fun i ally -> (AllyIndex i, ally)) allies @
                     List.mapi (fun i enemy -> (EnemyIndex i, enemy)) enemies)
             { BattleState = BattleReady 0L
               Characters = characters
               CurrentCommandOpt = None
               ActionCommands = Queue.empty
               Inventory = { Items = Map.ofList [(Consumable GreenHerb, 2); (Consumable RedHerb, 2)] }
               Gold = 100 })

        static let tickAttack sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            let source = BattleModel.getCharacter sourceIndex model
            match targetIndexOpt with
            | Some targetIndex ->
                let target = BattleModel.getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.CharacterState.IsHealthy
                    then withMsg model (AttackCharacter sourceIndex)
                    else withMsgs { model with CurrentCommandOpt = None } [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | _ ->
                    if timeLocal = int64 source.AnimationState.Stutter
                    then withMsg model (DamageCharacter (sourceIndex, targetIndex, None))
                    elif CharacterAnimationState.finished time source.AnimationState then
                        let target = BattleModel.getCharacter targetIndex model
                        if target.CharacterState.IsHealthy then
                            let model = { model with CurrentCommandOpt = None }
                            withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                        else
                            let woundCommand = CurrentCommand.make time (ActionCommand.make Wound sourceIndex (Some targetIndex))
                            let model = { model with CurrentCommandOpt = Some woundCommand }
                            withMsg model (PoiseCharacter sourceIndex)
                    else just model
            | None ->
                let model = { model with CurrentCommandOpt = None }
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickConsume consumable sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            match targetIndexOpt with
            | Some targetIndex ->
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.CharacterState.IsHealthy
                    then withMsg model (GiveConsumable (consumable, sourceIndex))
                    else withMsgs { model with CurrentCommandOpt = None } [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | _ ->
                    if timeLocal = int64 source.AnimationState.Stutter * 3L
                    then withMsg model (TakeConsumable (consumable, targetIndex))
                    elif CharacterAnimationState.finished time target.AnimationState then
                        let model = { model with CurrentCommandOpt = None }
                        withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                    else just model
            | None ->
                let model = { model with CurrentCommandOpt = None }
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickSpecial specialType sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            let source = BattleModel.getCharacter sourceIndex model
            match targetIndexOpt with
            | Some targetIndex ->
                let target = BattleModel.getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.CharacterState.IsHealthy
                    then withMsg model (ChargeCharacter sourceIndex)
                    else withMsgs { model with CurrentCommandOpt = None } [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | _ ->
                    if timeLocal = int64 source.AnimationState.Stutter * 4L then
                        match specialType with
                        | HeadSlash -> withMsg model (AttackCharacter sourceIndex)
                        | Bolt -> withMsg model (AttackCharacter sourceIndex)
                    elif timeLocal = int64 source.AnimationState.Stutter * 5L then
                        withMsg model (DamageCharacter (sourceIndex, targetIndex, Some specialType))
                    elif CharacterAnimationState.finished time source.AnimationState then
                        let target = BattleModel.getCharacter targetIndex model
                        if target.CharacterState.IsHealthy then
                            let model = { model with CurrentCommandOpt = None }
                            withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                        else
                            let woundCommand = CurrentCommand.make time (ActionCommand.make Wound sourceIndex (Some targetIndex))
                            let model = { model with CurrentCommandOpt = Some woundCommand }
                            withMsg model (PoiseCharacter sourceIndex)
                    else just model
            | None ->
                let model = { model with CurrentCommandOpt = None }
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickWound targetIndex time model =
            let character = BattleModel.getCharacter targetIndex model
            if character.CharacterState.IsAlly then
                match character.AnimationState.AnimationCycle with
                | DamageCycle ->
                    if CharacterAnimationState.finished time character.AnimationState
                    then withMsg { model with CurrentCommandOpt = None } (WoundCharacter targetIndex)
                    else just model
                | _ -> failwithumf ()
            else
                match character.AnimationState.AnimationCycle with
                | DamageCycle ->
                    if CharacterAnimationState.finished time character.AnimationState then
                        let woundCharacter = WoundCharacter targetIndex
                        let playDeathSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.DeathSound)
                        withSigs model [Message woundCharacter; Command playDeathSound]
                    else just model
                | WoundCycle ->
                    if CharacterAnimationState.finished time character.AnimationState
                    then withMsg { model with CurrentCommandOpt = None } (DestroyCharacter targetIndex)
                    else just model
                | _ -> failwithumf ()

        static let tickReady time timeStart model =
            let timeLocal = time - timeStart
            match timeLocal with
            | 0L -> withMsg model ReadyCharacters
            | 30L -> withMsg { model with BattleState = BattleRunning } PoiseCharacters
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
                            if Seq.forall (fun character -> character.CharacterState.IsWounded) allies
                            then tick time { model with BattleState = BattleCease (false, time) } // tick for frame 0
                            elif Seq.forall (fun character -> character.CharacterState.IsWounded) enemies
                            then tick time { model with BattleState = BattleCease (true, time) } // tick for frame 0
                            else just model
                        withSig model (signal + signal2)
                | None -> just model

        and tickNoCommand time model =
            match model.ActionCommands with
            | Queue.Cons (currentCommand, nextCommands) ->
                let command = CurrentCommand.make time currentCommand
                let model = { model with CurrentCommandOpt = Some command; ActionCommands = nextCommands }
                tick time model // tick for frame 0
            | Queue.Nil ->
                let (allySignalsRev, model) =
                    List.fold (fun (signals, model) ally ->
                        if ally.ActionTime = Constants.Battle.ActionTime then
                            let model =
                                BattleModel.updateCharacter
                                    (fun character ->
                                        let characterState = character.CharacterState
                                        let characterState =
                                            if characterState.Defending
                                            then { characterState with CounterBuff = max 0.0f (characterState.CounterBuff - Constants.Battle.DefendingCounterBuff) }
                                            else characterState
                                        let characterState = { characterState with Defending = false }
                                        let character = { character with CharacterState = characterState; InputState = RegularMenu }
                                        let character = CharacterModel.setAnimationCycle time (PoiseCycle Poising) character
                                        character)
                                    ally.CharacterState.CharacterIndex
                                    model
                            let playActionTimeSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.AffirmSound)
                            (Command playActionTimeSound :: signals, model)
                        else (signals, model))
                        ([], model)
                        (BattleModel.getAllies model)
                let (enemySignalsRev, model) =
                    List.fold (fun (signals, model) enemy ->
                        if enemy.ActionTime = Constants.Battle.ActionTime then
                            let enemyIndex = EnemyIndex enemy.CharacterState.PartyIndex
                            match enemy.CharacterState.AutoBattleOpt with
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
                        let character = CharacterModel.changeActionTime Constants.Battle.ActionTimeInc character
                        let character =
                            if CharacterModel.readyForAutoBattle character
                            then BattleModel.runAutoBattle character model
                            else character
                        character)
                        model
                withSigs model (List.rev (allySignalsRev @ enemySignalsRev))

        and tickRunning time model =
            match model.CurrentCommandOpt with
            | Some currentCommand -> tickCurrentCommand time currentCommand model
            | None -> tickNoCommand time model

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
                    | "Attack" -> BattleModel.updateCharacter (CharacterModel.setInputState (AimReticles (item, EnemyAim))) characterIndex model
                    | "Special" -> BattleModel.updateCharacter (CharacterModel.setInputState SpecialMenu) characterIndex model
                    | "Item" -> BattleModel.updateCharacter (CharacterModel.setInputState ItemMenu) characterIndex model
                    | "Defend" ->
                        BattleModel.updateCharacter
                            (fun character ->
                                let time = World.getTickTime world
                                let characterState = character.CharacterState
                                let characterState = { characterState with Defending = true }
                                let characterState = { characterState with CounterBuff = characterState.CounterBuff + Constants.Battle.DefendingCounterBuff }
                                let character = { character with CharacterState = characterState; ActionTime = 0; InputState = NoInput }
                                let character = CharacterModel.setAnimationCycle time (PoiseCycle Defending) character
                                character)
                            characterIndex
                            model
                    | _ -> failwithumf ()
                just model
            | RegularItemCancel characterIndex ->
                let model = BattleModel.updateCharacter (CharacterModel.setInputState RegularMenu) characterIndex model
                just model
            | SpecialItemSelect (characterIndex, item) ->
                let model = BattleModel.updateCharacter (CharacterModel.setInputState (AimReticles (item, EnemyAim))) characterIndex model
                just model
            | SpecialItemCancel characterIndex ->
                let model = BattleModel.updateCharacter (CharacterModel.setInputState RegularMenu) characterIndex model
                just model
            | ItemItemSelect (characterIndex, item) ->
                let model = BattleModel.updateCharacter (CharacterModel.setInputState (AimReticles (item, AllyAimHealthy))) characterIndex model
                just model
            | ItemItemCancel characterIndex ->
                let model = BattleModel.updateCharacter (CharacterModel.setInputState RegularMenu) characterIndex model
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
                            | "Bolt" -> ActionCommand.make (Special Bolt) allyIndex (Some targetIndex)
                            | _ -> ActionCommand.make Attack allyIndex (Some targetIndex)
                        let model = BattleModel.conjActionCommand command model
                        withMsg model (ResetCharacter allyIndex)
                    | _ -> just model
                | _ -> just model
            | ReticlesCancel characterIndex ->
                let model = BattleModel.updateCharacter (CharacterModel.setInputState RegularMenu) characterIndex model
                just model
            | ReadyCharacters ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacters (CharacterModel.setAnimationCycle time ReadyCycle) model
                just model
            | PoiseCharacters ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacters
                        (fun character ->
                            let poiseType = CharacterState.getPoiseType character.CharacterState
                            let character = CharacterModel.setAnimationCycle time (PoiseCycle poiseType) character
                            character)
                        model
                just model
            | CelebrateCharacters outcome ->
                let time = World.getTickTime world
                let model =
                    if outcome
                    then BattleModel.updateAllies (CharacterModel.setAnimationCycle time CelebrateCycle) model
                    else BattleModel.updateEnemies (CharacterModel.setAnimationCycle time CelebrateCycle) model
                just model
            | AttackCharacter sourceIndex ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.setAnimationCycle time AttackCycle) sourceIndex model
                let playHitSoundDelay = int64 (BattleModel.getCharacter sourceIndex model).AnimationState.Stutter
                let playHitSound = PlaySound (playHitSoundDelay, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                withCmd model playHitSound
            | DamageCharacter (sourceIndex, targetIndex, specialTypeOpt) ->
                let time = World.getTickTime world
                let rom = screen.Parent.GetModel<Rom> world
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                let (cancelled, damage, model) =
                    match specialTypeOpt with
                    | None ->
                        let (cancelled, damage) = CharacterState.getAttackResult rom false 1 source.CharacterState target.CharacterState
                        let model = BattleModel.updateCharacterState (CharacterState.changeHitPoints rom cancelled -damage) targetIndex model
                        (cancelled, damage, model)
                    | Some HeadSlash ->
                        let (cancelled, damage) = CharacterState.getAttackResult rom true 1 source.CharacterState target.CharacterState
                        let model = BattleModel.updateCharacterState (CharacterState.changeHitPoints rom cancelled -damage) targetIndex model
                        (cancelled, damage, model)
                    | Some Bolt ->
                        (false, 0, model) // TODO: implement
                let (model, sigs) =
                    if target.CharacterState.HitPoints <= 0
                    then (model, [Message (ResetCharacter targetIndex)]) // wounded
                    else (BattleModel.updateCharacter (CharacterModel.setAnimationCycle time DamageCycle) targetIndex model, []) // just damaged
                let sigs = if cancelled then Command (DisplayCancel targetIndex) :: sigs else sigs
                let sigs = Command (DisplayHitPointsChange (targetIndex, -damage)) :: sigs
                withSigs model sigs
                
            | ChargeCharacter sourceIndex ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.setAnimationCycle time (PoiseCycle Charging)) sourceIndex model
                just model
            | PoiseCharacter characterIndex ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacter
                        (fun character ->
                            let poiseType = CharacterState.getPoiseType character.CharacterState
                            let character = CharacterModel.setAnimationCycle time (PoiseCycle poiseType) character
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
                                if character.CharacterState.IsAlly
                                then CharacterModel.setInputState NoInput character
                                else character
                            let character = CharacterModel.setAnimationCycle time WoundCycle character
                            character)
                        characterIndex
                        model
                just model
            | ResetCharacter characterIndex ->
                let character = BattleModel.getCharacter characterIndex model
                let model = BattleModel.updateCharacter (CharacterModel.setActionTime 0) characterIndex model
                let model =
                    if character.CharacterState.IsAlly
                    then BattleModel.updateCharacter (CharacterModel.setInputState NoInput) characterIndex model
                    else BattleModel.updateCharacterState (CharacterState.setAutoBattleOpt None) characterIndex model
                just model
            | DestroyCharacter characterIndex ->
                let character = BattleModel.getCharacter characterIndex model
                let model =
                    if character.CharacterState.IsEnemy
                    then { model with Characters = Map.remove characterIndex model.Characters }
                    else model
                just model
            | GiveConsumable (consumable, sourceIndex) ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.setAnimationCycle time CastCycle) sourceIndex model
                let item = Consumable consumable
                let model = { model with Inventory = Inventory.removeItem item model.Inventory }
                just model
            | TakeConsumable (consumable, targetIndex) ->
                let time = World.getTickTime world
                let rom = screen.Parent.GetModel<Rom> world
                let healing =
                    match consumable with
                    | GreenHerb -> 50 // TODO: pull from rom data
                    | RedHerb -> 500 // TODO: pull from rom data
                let model = BattleModel.updateCharacterState (CharacterState.changeHitPoints rom false healing) targetIndex model
                let model = BattleModel.updateCharacter (CharacterModel.setAnimationCycle time SpinCycle) targetIndex model
                let displayHitPointsChange = DisplayHitPointsChange (targetIndex, healing)
                let playHealSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.HealSound)
                withCmds model [displayHitPointsChange; playHealSound]
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
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let effect =
                        { EffectName = "Cancel"
                          LifetimeOpt = Some 40L
                          Definitions = Map.empty
                          Content =
                            Effects.StaticSprite
                                (Effects.Resource (Assets.CancelImage.PackageName, Assets.CancelImage.AssetName),
                                 [|Effects.Rotation
                                    (Effects.Sum, Effects.Linear, Effects.Bounce,
                                     [|{ TweenValue = single Math.PI * -2.0f; TweenLength = 10L }
                                       { TweenValue = 0.0f; TweenLength = 30L }
                                       { TweenValue = 0.0f; TweenLength = 0L }|])
                                   Effects.Size
                                    (Effects.Set, Effects.EaseOut, Effects.Once,
                                     [|{ TweenValue = v2Zero; TweenLength = 10L }
                                       { TweenValue = v2 208.0f 64.0f; TweenLength = 30L }
                                       { TweenValue = v2 208.0f 64.0f; TweenLength = 0L }|])|],
                                 Effects.Nil) }
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset3 world
                    let world = entity.SetDepth (Constants.Battle.GuiEffectDepth - 1.0f) world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world
            | DisplayHitPointsChange (targetIndex, delta) ->
                match BattleModel.tryGetCharacter targetIndex model with
                | Some target ->
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let colorOpaque =
                        if delta < 0
                        then v4 1.0f 1.0f 1.0f 1.0f
                        else v4 0.0f 1.0f 1.0f 1.0f
                    let colorTransparent =
                        colorOpaque.WithW 0.0f
                    let effect =
                        { EffectName = "HitPointsChange"
                          LifetimeOpt = Some 70L
                          Definitions = Map.empty
                          Content =
                            Effects.TextSprite
                                (Effects.Resource (Assets.DefaultPackageName, Assets.DefaultFontName),
                                 [|Effects.Text (scstring (abs delta))
                                   Effects.Position
                                    (Effects.Sum, Effects.Linear, Effects.Bounce,
                                     [|{ TweenValue = v2Zero; TweenLength = 10L }
                                       { TweenValue = v2 0.0f 48.0f; TweenLength = 10L }
                                       { TweenValue = v2Zero; TweenLength = 10L }
                                       { TweenValue = v2Zero; TweenLength = 40L }|])
                                   Effects.Color
                                    (Effects.Set, Effects.EaseOut, Effects.Once,
                                     [|{ TweenValue = colorOpaque; TweenLength = 40L }
                                       { TweenValue = colorOpaque; TweenLength = 30L }
                                       { TweenValue = colorTransparent; TweenLength = 0L }|])|],
                                 Effects.Nil) }
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset2 world
                    let world = entity.SetDepth Constants.Battle.GuiEffectDepth world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world
            | InitializeBattle ->
                let world = World.hintRenderPackageUse Assets.BattlePackageName world
                let world = World.hintAudioPackageUse Assets.BattlePackageName world
                let world = World.playSong 0 Constants.Audio.DefaultSongVolume Assets.BattleSong world
                let world = battle.SetBattleModel { model with BattleState = BattleReady (World.getTickTime world) } world
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
                    (fun model -> model.CharacterState.PartyIndex)
                    (fun index model _ -> Content.entity<CharacterDispatcher> ("Ally+" + scstring index) [Entity.CharacterModel <== model])
                 Content.entitiesIndexedBy
                    (model --> fun model -> BattleModel.getEnemies model)
                    (fun model -> model.CharacterState.PartyIndex)
                    (fun index model _ -> Content.entity<CharacterDispatcher> ("Enemy+" + scstring index) [Entity.CharacterModel <== model])]

        member private this.InputContent (model : Lens<BattleModel, World>, screen : Screen, _ : World) =
            Content.layers (model --> fun model -> BattleModel.getAllies model) $ fun index ally _ ->
                let allyIndex = AllyIndex index
                let input = screen / ("Input" + "+" + scstring index)
                Content.layer input.Name []
                    [Content.fillBar "HealthBar" 
                        [Entity.Size == v2 64.0f 8.0f
                         Entity.Center <== ally --> fun ally -> ally.UnderFeet
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.CharacterState.HitPoints > 0
                         Entity.Fill <== ally ->> fun ally world ->
                            let rom = screen.Parent.GetModel<Rom> world
                            single ally.CharacterState.HitPoints / single (ally.CharacterState.HitPointsMax rom)]
                     Content.entity<RingMenuDispatcher> "RegularMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = RegularMenu
                         Entity.Enabled <== model --> fun model ->
                            let allies = BattleModel.getAllies model
                            let alliesPastRegularMenu = List.notExists (fun ally -> match ally.InputState with NoInput | RegularMenu -> false | _ -> true) allies
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
                            let specials = List.ofSeq ally.CharacterState.Specials
                            let specials =
                                List.map (fun special ->
                                    let specialTag = getTag special
                                    let specialUsable =
                                        match Map.tryFind special rom.Specials with
                                        | Some specialData -> specialData.SpecialCost <= ally.CharacterState.SpecialPoints
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
                         Entity.ReticlesModel <== model --> fun model -> { Characters = model.Characters; AimType = (BattleModel.getCharacter allyIndex model).InputState.AimType }
                         Entity.TargetSelectEvent ==|> fun evt -> msg (ReticlesSelect (evt.Data, allyIndex))
                         Entity.CancelEvent ==> msg (ReticlesCancel allyIndex)]]

        override this.Content (model, screen, world) =
            [this.SceneContent (model, screen, world)
             this.InputContent (model, screen, world)]