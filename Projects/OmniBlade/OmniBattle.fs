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
        | AdvanceCharacters
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
                [{ CharacterState = { CharacterType = Ally Jinn; PartyIndex = 0; ExpPoints = 0; HitPoints = 15; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [JumpSlash; Volt]; Statuses = Set.empty; WeaponOpt = Some "Wooden Sword"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Jinn"; AnimationCycle = ReadyCycle; Direction = Rightward; Stutter = 10 }
                   ActionTime = 600
                   AutoBattleOpt = None
                   InputState = NoInput
                   Position = v2 -224.0f -168.0f
                   Size = v2 160.0f 160.0f }
                 { CharacterState = { CharacterType = Ally Glenn; PartyIndex = 1; ExpPoints = 0; HitPoints = 15; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.ofList [JumpSlash; Volt]; Statuses = Set.empty; WeaponOpt = Some "Wooden Sword"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Glenn"; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   ActionTime = 300
                   AutoBattleOpt = None
                   InputState = NoInput
                   Position = v2 224.0f 64.0f
                   Size = v2 160.0f 160.0f }]
             let enemies =
                [{ CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.empty; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Goblin"; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   ActionTime = 0
                   AutoBattleOpt = None
                   InputState = NoInput
                   Position = v2 0.0f 0.0f
                   Size = v2 160.0f 160.0f }
                 { CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 1; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.empty; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Goblin"; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   ActionTime = 0
                   AutoBattleOpt = None
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
                let target = BattleModel.getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.CharacterState.IsHealthy
                    then withMsg model (GiveConsumable (consumable, sourceIndex))
                    else withMsgs { model with CurrentCommandOpt = None } [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | _ ->
                    if timeLocal = int64 target.AnimationState.Stutter
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
                        | JumpSlash -> withMsg model (AttackCharacter sourceIndex)
                        | Volt -> withMsg model (AttackCharacter sourceIndex)
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
                    if CharacterAnimationState.finished time character.AnimationState
                    then withMsg model (WoundCharacter targetIndex)
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
                let model =
                    List.fold (fun model ally ->
                        if ally.ActionTime = Constants.Battle.ActionTime then
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
                        else model)
                        model
                        (BattleModel.getAllies model)
                let (enemySignalsRev, model) =
                    List.fold (fun (commands, model) enemy ->
                        if enemy.ActionTime = Constants.Battle.ActionTime then
                            let enemyIndex = EnemyIndex enemy.CharacterState.PartyIndex
                            match enemy.AutoBattleOpt with
                            | Some autoBattle ->
                                let attack = { Action = Attack; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                                let model = BattleModel.conjActionCommand attack model
                                (Message (ResetCharacter enemyIndex) :: commands, model)
                            | None -> (Message (ResetCharacter enemyIndex) :: commands, model)
                        else (commands, model))
                        ([], model)
                        (BattleModel.getEnemies model)
                let advanceCharactersSignal = Message AdvanceCharacters
                let signals = advanceCharactersSignal :: List.rev enemySignalsRev
                withSigs model signals

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

        override this.Message (model, message, _, world) =
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
                let model = BattleModel.updateCharacter (CharacterModel.setInputState (AimReticles (item, AllyAim true))) characterIndex model
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
                            | "JumpSlash" -> ActionCommand.make (Special JumpSlash) allyIndex (Some targetIndex)
                            | "Volt" -> ActionCommand.make (Special Volt) allyIndex (Some targetIndex)
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
                            let poiseType = CharacterModel.getPoiseType character
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
            | AdvanceCharacters ->
                let model =
                    BattleModel.updateCharacters (fun character ->
                        let character = CharacterModel.changeActionTime Constants.Battle.ActionTimeInc character
                        let character =
                            if CharacterModel.readyForAutoBattle character
                            then BattleModel.runAutoBattle character model
                            else character
                        character)
                        model
                just model
            | AttackCharacter sourceIndex ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.setAnimationCycle time AttackCycle) sourceIndex model
                just model
            | DamageCharacter (sourceIndex, targetIndex, specialTypeOpt) ->
                let time = World.getTickTime world
                let rom = Simulants.Game.GetModel world
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                let (damage, model) =
                    match specialTypeOpt with
                    | None ->
                        let damage = CharacterState.getDamage 1 source.CharacterState target.CharacterState rom
                        let model = BattleModel.updateCharacter (CharacterModel.changeHitPoints -damage) targetIndex model
                        (damage, model)
                    | Some JumpSlash ->
                        let damage = CharacterState.getDamage 2 source.CharacterState target.CharacterState rom // TODO: pull scalar from rom
                        let model = BattleModel.updateCharacter (CharacterModel.changeHitPoints -damage) targetIndex model
                        (damage, model)
                    | Some Volt ->
                        (0, model) // TODO: implement
                if target.CharacterState.HitPoints > 0 || target.CharacterState.IsEnemy then
                    let model = BattleModel.updateCharacter (CharacterModel.setAnimationCycle time DamageCycle) targetIndex model
                    withCmd model (DisplayHitPointsChange (targetIndex, -damage))
                else withMsg model (ResetCharacter targetIndex)
            | ChargeCharacter sourceIndex ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.setAnimationCycle time (PoiseCycle Charging)) sourceIndex model
                just model
            | PoiseCharacter characterIndex ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacter
                        (fun character ->
                            let poiseType = CharacterModel.getPoiseType character
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
                    else BattleModel.updateCharacter (CharacterModel.setAutoBattleOpt None) characterIndex model
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
                let healing =
                    match consumable with
                    | GreenHerb -> 100 // TODO: pull from rom data
                    | RedHerb -> 500 // TODO: pull from rom data
                let model = BattleModel.updateCharacter (CharacterModel.changeHitPoints healing) targetIndex model
                let model = BattleModel.updateCharacter (CharacterModel.setAnimationCycle time SpinCycle) targetIndex model
                withCmd model (DisplayHitPointsChange (targetIndex, healing))
            | Tick ->
                if World.isTicking world
                then tick (World.getTickTime world) model
                else just model

        override this.Command (model, command, battle, world) =
            match command with
            | FadeSong ->
                let world = World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
                just world
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
                        { EffectName = ""
                          LifetimeOpt = Some 60L
                          Definitions = Map.empty
                          Content =
                            Effects.TextSprite
                                (Effects.Resource (Assets.DefaultPackage, Assets.DefaultFont),
                                 [|Effects.Text (scstring (abs delta))
                                   Effects.Color
                                    (Effects.Set, Effects.Linear, Effects.Once,
                                     [|{ TweenValue = colorOpaque; TweenLength = 30L }
                                       { TweenValue = colorOpaque; TweenLength = 30L }
                                       { TweenValue = colorTransparent; TweenLength = 0L }|])|],
                                 Effects.Nil) }
                    let world = entity.SetEffect effect world
                    let world = entity.SetSize v2Zero world // TODO: figure out why we have to set size to zero instead of just setting center
                    let world = entity.SetPosition target.Bottom world
                    let world = entity.SetDepth 100.0f world // TODO: derive this from something understandable
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world
            | InitializeBattle ->
                let world = World.hintRenderPackageUse Assets.BattlePackage world
                let world = World.hintAudioPackageUse Assets.BattlePackage world
                let world = World.playSong 0 (1.0f * Constants.Audio.MasterSongVolume) Assets.BattleSong world
                let world = battle.SetBattleModel { model with BattleState = BattleReady (World.getTickTime world) } world
                just world
            | FinalizeBattle ->
                let world = World.hintRenderPackageDisuse Assets.BattlePackage world
                just (World.hintAudioPackageDisuse Assets.BattlePackage world)

        member private this.SceneContent (model : Lens<BattleModel, World>, _ : Screen, _ : World) =
            let background = Simulants.BattleScene / "Background"
            Content.layer Simulants.BattleScene.Name []
                [Content.label background.Name
                    [background.Position == v2 -480.0f -512.0f
                     background.Size == v2 1024.0f 1024.0f
                     background.Depth == -10.0f
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
                    [Content.entity<RingMenuDispatcher> "RegularMenu"
                        [Entity.Position <== ally --> fun ally -> ally.Center + Constants.Battle.CharacterCenterOffset
                         Entity.Depth == 10.0f
                         Entity.Visible <== ally --> fun ally -> ally.InputState = RegularMenu
                         Entity.RingMenuModel == { Items = [(0, "Attack"); (1, "Defend"); (2, "Item"); (3, "Special")]; ItemCancelOpt = None }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (RegularItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (RegularItemCancel allyIndex)]
                     Content.entity<RingMenuDispatcher> "SpecialMenu"
                        [Entity.Position <== ally --> fun ally -> ally.Center + Constants.Battle.CharacterCenterOffset
                         Entity.Depth == 10.0f
                         Entity.Visible <== ally --> fun ally -> ally.InputState = SpecialMenu
                         Entity.RingMenuModel <== ally --> fun ally ->
                             let specials = List.ofSeq ally.CharacterState.Specials
                             let specials = List.map (fun special -> (getTag special, scstring special)) specials
                             { Items = specials; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (SpecialItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (SpecialItemCancel allyIndex)]
                     Content.entity<RingMenuDispatcher> "ItemMenu"
                        [Entity.Position <== ally --> fun ally -> ally.Center + Constants.Battle.CharacterCenterOffset
                         Entity.Depth == 10.0f
                         Entity.Visible <== ally --> fun ally -> ally.InputState = ItemMenu
                         Entity.RingMenuModel <== model --> fun model ->
                            let consumables = Inventory.getConsumables model.Inventory
                            let consumables = Map.toKeyList consumables
                            let consumables = List.map (fun special -> (getTag special, scstring special)) consumables
                            { Items = consumables; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (ItemItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (ItemItemCancel allyIndex)]
                     Content.entity<ReticlesDispatcher> "Reticles"
                        [Entity.Position <== ally --> fun ally -> ally.Center + Constants.Battle.CharacterCenterOffset
                         Entity.Depth == 10.0f
                         Entity.Visible <== ally --> fun ally -> match ally.InputState with AimReticles _ -> true | _ -> false
                         Entity.ReticlesModel <== model --> fun model -> { Characters = model.Characters; AimType = (BattleModel.getCharacter allyIndex model).InputState.AimType }
                         Entity.TargetSelectEvent ==|> fun evt -> msg (ReticlesSelect (evt.Data, allyIndex))
                         Entity.CancelEvent ==> msg (ReticlesCancel allyIndex)]]

        override this.Content (model, screen, world) =
            [this.SceneContent (model, screen, world)
             this.InputContent (model, screen, world)]