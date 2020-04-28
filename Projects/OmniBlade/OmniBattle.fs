namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniBattle =

    type [<NoComparison>] BattleMessage =
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

    type [<NoComparison>] BattleCommand =
        | FadeSong
        | InitializeBattle
        | FinalizeBattle

    type Screen with

        member this.GetBattleModel = this.GetModel<BattleModel>
        member this.SetBattleModel = this.SetModel<BattleModel>
        member this.BattleModel = this.Model<BattleModel> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<BattleModel, BattleMessage, BattleCommand>
            (let allies =
                [{ CharacterState = { CharacterType = Ally Jinn; PartyIndex = 0; ActionTime = 600; ExpPoints = 0; HitPoints = 15; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.singleton JumpSlash; Statuses = Set.empty; WeaponOpt = Some "Wooden Sword"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Jinn"; AnimationCycle = ReadyCycle; Direction = Rightward; Stutter = 10 }
                   InputState = NoInput
                   Position = v2 -224.0f -168.0f
                   Size = v2 160.0f 160.0f }]
             let enemies =
                [{ CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 0; ActionTime = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.empty; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Goblin"; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   InputState = NoInput
                   Position = v2 0.0f 64.0f
                   Size = v2 160.0f 160.0f }
                 { CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 1; ActionTime = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.empty; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Goblin"; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   InputState = NoInput
                   Position = v2 176.0f -152.0f
                   Size = v2 160.0f 160.0f }]
             let characters =
                Map.ofList
                    (List.mapi (fun i ally -> (AllyIndex i, ally)) allies @
                     List.mapi (fun i enemy -> (EnemyIndex i, enemy)) enemies)
             { BattleState = BattleReady 0L
               Characters = characters
               CurrentCommandOpt = None
               ActionQueue = Queue.empty
               Inventory = { Items = Map.ofList [(Consumable GreenHerb, 3); (Consumable RedHerb, 1)] }
               Gold = 100 })

        static let getAllies model =
            CharacterModels.getAllies model.Characters

        static let getEnemies model =
            CharacterModels.getEnemies model.Characters

        static let updateCharactersIf predicate updater model =
            { model with BattleModel.Characters = Map.map (fun index character -> if predicate index then updater character else character) model.Characters }

        static let updateCharacters updater model =
            updateCharactersIf tautology updater model

        static let updateAllies updater model =
            updateCharactersIf (function AllyIndex _ -> true | _ -> false) updater model

        static let updateEnemies updater model =
            updateCharactersIf (function EnemyIndex _ -> true | _ -> false) updater model

        static let tryGetCharacter characterIndex model =
            Map.tryFind characterIndex model.Characters

        static let getCharacter characterIndex model =
            tryGetCharacter characterIndex model |> Option.get

        static let tryUpdateCharacter updater characterIndex model =
            match tryGetCharacter characterIndex model with
            | Some character ->
                let character = updater character
                { model with Characters = Map.add characterIndex character model.Characters }
            | None -> model

        static let updateCharacter updater characterIndex model =
            do ignore tryUpdateCharacter // temporarily quiet error about tryUpdateCharacter being unused
            let character = getCharacter characterIndex model
            let character = updater character
            { model with Characters = Map.add characterIndex character model.Characters }

        static let getCharacterPoiseType character =
            if character.CharacterState.Defending then Defending
            elif character.CharacterState.Charging then Charging
            else Poising

        static let tickAttack sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            let source = getCharacter sourceIndex model
            match targetIndexOpt with
            | Some targetIndex ->
                let target = getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.CharacterState.IsHealthy
                    then withMsg model (AttackCharacter sourceIndex)
                    else withMsgs { model with CurrentCommandOpt = None } [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | _ ->
                    if timeLocal = int64 source.AnimationState.Stutter
                    then withMsg model (DamageCharacter (sourceIndex, targetIndex, None))
                    elif CharacterAnimationState.finished time source.AnimationState then
                        let target = getCharacter targetIndex model
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
            let source = getCharacter sourceIndex model
            match targetIndexOpt with
            | Some targetIndex ->
                let target = getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.CharacterState.IsHealthy
                    then withMsg model (GiveConsumable (consumable, sourceIndex))
                    else withMsgs { model with CurrentCommandOpt = None } [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | _ ->
                    if timeLocal = int64 source.AnimationState.Stutter
                    then withMsg model (TakeConsumable (consumable, targetIndex))
                    elif CharacterAnimationState.finished time source.AnimationState then
                        let model = { model with CurrentCommandOpt = None }
                        withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                    else just model
            | None ->
                let model = { model with CurrentCommandOpt = None }
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickSpecial specialType sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            let source = getCharacter sourceIndex model
            match targetIndexOpt with
            | Some targetIndex ->
                let target = getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.CharacterState.IsHealthy
                    then withMsg model (ChargeCharacter sourceIndex)
                    else withMsgs { model with CurrentCommandOpt = None } [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | _ ->
                    if timeLocal = int64 source.AnimationState.Stutter then
                        match specialType with
                        | JumpSlash -> withMsg model (AttackCharacter sourceIndex)
                    elif timeLocal = int64 source.AnimationState.Stutter * 2L then
                        withMsg model (DamageCharacter (sourceIndex, targetIndex, Some specialType))
                    elif CharacterAnimationState.finished time source.AnimationState then
                        let target = getCharacter targetIndex model
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
            let character = getCharacter targetIndex model
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
                            let allies = getAllies model
                            let enemies = getEnemies model
                            if Seq.forall (fun character -> character.CharacterState.IsWounded) allies
                            then tick time { model with BattleState = BattleCease (false, time) } // tick for frame 0
                            elif Seq.forall (fun character -> character.CharacterState.IsWounded) enemies
                            then tick time { model with BattleState = BattleCease (true, time) } // tick for frame 0
                            else just model
                        withSig model (signal + signal2)
                | None -> just model

        and tickNoCommand time model =
            match model.ActionQueue with
            | Queue.Cons (currentCommand, nextCommands) ->
                let command = CurrentCommand.make time currentCommand
                let model = { model with CurrentCommandOpt = Some command; ActionQueue = nextCommands }
                tick time model // tick for frame 0
            | Queue.Nil ->
                let model =
                    List.fold (fun model ally ->
                        if ally.CharacterState.ActionTime = Constants.Battle.ActionTime then
                            updateCharacter
                                (fun character ->
                                    let characterState = character.CharacterState
                                    let characterState =
                                        if characterState.Defending
                                        then { characterState with CounterBuff = max 0.0f (characterState.CounterBuff - Constants.Battle.DefendingCounterBuff) }
                                        else characterState
                                    let characterState = { characterState with Defending = false }
                                    let animationState = { character.AnimationState with AnimationCycle = PoiseCycle Poising }
                                    let inputState = RegularMenu
                                    { character with CharacterState = characterState; AnimationState = animationState; InputState = inputState })
                                ally.CharacterState.CharacterIndex
                                model
                        else model)
                        model
                        (getAllies model)
                let (enemySignalsRev, model) =
                    List.fold (fun (commands, model) enemy ->
                        if enemy.CharacterState.ActionTime = Constants.Battle.ActionTime then
                            let enemyIndex = EnemyIndex enemy.CharacterState.PartyIndex
                            let allies = getAllies model
                            let allyIndex = (Random ()).Next allies.Length
                            let attack = { Action = Attack; Source = enemyIndex; TargetOpt = Some (AllyIndex allyIndex) }
                            let model = { model with ActionQueue = Queue.conj attack model.ActionQueue }
                            (Message (ResetCharacter enemyIndex) :: commands, model)
                        else (commands, model))
                        ([], model)
                        (getEnemies model)
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
                    | "Attack" ->
                        updateCharacter (fun character -> { character with InputState = AimReticles (item, EnemyAim) }) characterIndex model
                    | "Special" ->
                        updateCharacter (fun character -> { character with InputState = SpecialMenu }) characterIndex model
                    | "Item" ->
                        updateCharacter (fun character -> { character with InputState = ItemMenu }) characterIndex model
                    | "Defend" ->
                        updateCharacter
                            (fun character ->
                                let characterState = character.CharacterState
                                let characterState = { characterState with ActionTime = 0 }
                                let characterState = { characterState with Defending = true }
                                let characterState = { characterState with CounterBuff = characterState.CounterBuff + Constants.Battle.DefendingCounterBuff }
                                let animationState = { character.AnimationState with AnimationCycle = PoiseCycle Defending }
                                let inputState = NoInput
                                { character with CharacterState = characterState; AnimationState = animationState; InputState = inputState })
                            characterIndex
                            model
                    | _ ->
                        failwithumf ()
                just model
            | RegularItemCancel characterIndex ->
                let model = updateCharacter (fun character -> { character with InputState = RegularMenu }) characterIndex model
                just model
            | SpecialItemSelect (characterIndex, item) ->
                let model = updateCharacter (fun character -> { character with InputState = AimReticles (item, EnemyAim) }) characterIndex model
                just model
            | SpecialItemCancel characterIndex ->
                let model = updateCharacter (fun character -> { character with InputState = RegularMenu }) characterIndex model
                just model
            | ItemItemSelect (characterIndex, item) ->
                let model = updateCharacter (fun character -> { character with InputState = AimReticles (item, AllyAim true) }) characterIndex model
                just model
            | ItemItemCancel characterIndex ->
                let model = updateCharacter (fun character -> { character with InputState = RegularMenu }) characterIndex model
                just model
            | ReticlesSelect (targetIndex, allyIndex) ->
                match model.BattleState with
                | BattleRunning ->
                    let ally = getCharacter allyIndex model
                    match ally.InputState with
                    | AimReticles (item, _) ->
                        let command =
                            match item with
                            | "GreenHerb" -> ActionCommand.make (Consume GreenHerb) allyIndex (Some targetIndex)
                            | "RedHerb" -> ActionCommand.make (Consume RedHerb) allyIndex (Some targetIndex)
                            | "JumpSlash" -> ActionCommand.make (Special JumpSlash) allyIndex (Some targetIndex)
                            | _ -> ActionCommand.make Attack allyIndex (Some targetIndex)
                        let model = { model with ActionQueue = Queue.conj command model.ActionQueue }
                        withMsg model (ResetCharacter allyIndex)
                    | _ -> just model
                | _ -> just model
            | ReticlesCancel characterIndex ->
                let model = updateCharacter (fun character -> { character with InputState = RegularMenu }) characterIndex model
                just model
            | ReadyCharacters ->
                let time = World.getTickTime world
                let model = updateCharacters (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) ReadyCycle character.AnimationState }) model
                just model
            | PoiseCharacters ->
                let time = World.getTickTime world
                let model =
                    updateCharacters
                        (fun character ->
                            let poiseType = getCharacterPoiseType character
                            let animationState = CharacterAnimationState.setCycle (Some time) (PoiseCycle poiseType) character.AnimationState
                            { character with AnimationState = animationState })
                        model
                just model
            | CelebrateCharacters outcome ->
                let time = World.getTickTime world
                let model =
                    if outcome
                    then updateAllies (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) CelebrateCycle character.AnimationState }) model
                    else updateEnemies (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) CelebrateCycle character.AnimationState }) model
                just model
            | AdvanceCharacters ->
                let model = updateCharacters (fun character -> { character with CharacterState = { character.CharacterState with ActionTime = character.CharacterState.ActionTime + Constants.Battle.ActionTimeInc }}) model
                just model
            | AttackCharacter sourceIndex ->
                let time = World.getTickTime world
                let model = updateCharacter (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) AttackCycle character.AnimationState }) sourceIndex model
                just model
            | DamageCharacter (sourceIndex, targetIndex, specialTypeOpt) ->
                let time = World.getTickTime world
                let source = getCharacter sourceIndex model
                let target = getCharacter targetIndex model
                let model =
                    match specialTypeOpt with
                    | None ->
                        updateCharacter (fun character ->
                            let state = character.CharacterState
                            let rom = Simulants.Game.GetModel world
                            let power = source.CharacterState.ComputePower rom
                            let shield = state.ComputeShield rom
                            let damage = max 0 (int (Math.Ceiling (double (power - shield))))
                            let hitPoints = state.HitPoints
                            let hitPoints =  max 0 (hitPoints - damage)
                            { character with CharacterState = { state with HitPoints = hitPoints }})
                            targetIndex
                            model
                    | Some JumpSlash ->
                        updateCharacter (fun character ->
                            let state = character.CharacterState
                            let rom = Simulants.Game.GetModel world
                            let power = source.CharacterState.ComputePower rom * 3 // TODO: pull from rom data
                            let shield = state.ComputeShield rom
                            let damage = max 0 (int (Math.Ceiling (double (power - shield))))
                            let hitPoints = state.HitPoints
                            let hitPoints =  max 0 (hitPoints - damage)
                            { character with CharacterState = { state with HitPoints = hitPoints }})
                            targetIndex
                            model
                if target.CharacterState.HitPoints > 0 || target.CharacterState.IsEnemy then
                    let model = updateCharacter (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) DamageCycle character.AnimationState }) targetIndex model
                    just model
                else withSig model (Message (ResetCharacter targetIndex))
            | ChargeCharacter sourceIndex ->
                let time = World.getTickTime world
                let model = updateCharacter (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) (PoiseCycle Charging) character.AnimationState }) sourceIndex model
                just model
            | PoiseCharacter characterIndex ->
                let time = World.getTickTime world
                let model =
                    updateCharacter
                        (fun character ->
                            let poiseType = getCharacterPoiseType character
                            let animationState = CharacterAnimationState.setCycle (Some time) (PoiseCycle poiseType) character.AnimationState
                            { character with AnimationState = animationState })
                        characterIndex
                        model
                just model
            | WoundCharacter characterIndex ->
                let time = World.getTickTime world
                let model = updateCharacter (fun character -> if character.CharacterState.IsAlly then { character with InputState = NoInput } else character) characterIndex model
                let model = updateCharacter (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) WoundCycle character.AnimationState }) characterIndex model
                just model
            | ResetCharacter characterIndex ->
                let character = getCharacter characterIndex model
                let model = updateCharacter (fun character -> { character with CharacterState = { character.CharacterState with ActionTime = 0 }}) characterIndex model
                let model =
                    if character.CharacterState.IsAlly
                    then updateCharacter (fun character -> { character with InputState = NoInput }) characterIndex model
                    else model
                just model
            | DestroyCharacter characterIndex ->
                let character = getCharacter characterIndex model
                let model =
                    if character.CharacterState.IsEnemy
                    then { model with Characters = Map.remove characterIndex model.Characters }
                    else model
                just model
            | GiveConsumable (consumable, sourceIndex) ->
                let time = World.getTickTime world
                let model = updateCharacter (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) CastCycle character.AnimationState }) sourceIndex model
                let item = Consumable consumable
                match Map.tryFind item model.Inventory.Items with
                | Some itemCount when itemCount > 1 ->
                    let model = { model with Inventory = { model.Inventory with Items = Map.add item (dec itemCount) model.Inventory.Items }}
                    just model
                | Some itemCount when itemCount = 1 ->
                    let model = { model with Inventory = { model.Inventory with Items = Map.remove item  model.Inventory.Items }}
                    just model
                | _ -> just model
            | TakeConsumable (consumable, targetIndex) ->
                let time = World.getTickTime world
                let target = getCharacter targetIndex model
                let model =
                    updateCharacter (fun character ->
                        let state = character.CharacterState
                        match consumable with
                        | GreenHerb
                        | RedHerb ->
                            let cure =
                                match consumable with
                                | GreenHerb -> 100 // TODO: pull from rom data
                                | RedHerb -> 500 // TODO: pull from rom data
                            let hitPoints = state.HitPoints + cure
                            { character with CharacterState = { state with HitPoints = hitPoints }})
                        targetIndex
                        model
                if target.CharacterState.HitPoints > 0 || target.CharacterState.IsEnemy then
                    let model = updateCharacter (fun character -> { character with AnimationState = CharacterAnimationState.setCycle (Some time) SpinCycle character.AnimationState }) targetIndex model
                    just model
                else withSig model (Message (ResetCharacter targetIndex))
            | Tick ->
                if World.isTicking world
                then tick (World.getTickTime world) model
                else just model

        override this.Command (model, command, battle, world) =
            match command with
            | FadeSong ->
                let world = World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
                just world
            | InitializeBattle ->
                let world = World.hintRenderPackageUse Assets.BattlePackage world
                let world = World.hintAudioPackageUse Assets.BattlePackage world
                let world = World.playSong 0 (1.0f * Constants.Audio.MasterSongVolume) Assets.BattleSong world
                let world = battle.SetBattleModel { model with BattleState = BattleReady (World.getTickTime world) } world
                just world
            | FinalizeBattle ->
                let world = World.hintRenderPackageDisuse Assets.BattlePackage world
                just (World.hintAudioPackageDisuse Assets.BattlePackage world)

        member private this.SceneContent (model : Lens<BattleModel, World>, screen : Screen, _ : World) =
            let scene = screen / "Scene"
            let background = scene / "Background"
            Content.layer scene.Name []
                [Content.label background.Name
                    [background.Position == v2 -480.0f -512.0f
                     background.Size == v2 1024.0f 1024.0f
                     background.Depth == -10.0f
                     background.LabelImage == asset "Battle" "Background"]
                 Content.entitiesIndexedBy
                    (model --> fun model -> getAllies model)
                    (fun model -> model.CharacterState.PartyIndex)
                    (fun index model _ -> Content.entity<CharacterDispatcher> ("Ally+" + scstring index) [Entity.CharacterModel <== model])
                 Content.entitiesIndexedBy
                    (model --> fun model -> getEnemies model)
                    (fun model -> model.CharacterState.PartyIndex)
                    (fun index model _ -> Content.entity<CharacterDispatcher> ("Enemy+" + scstring index) [Entity.CharacterModel <== model])]

        member private this.InputContent (model : Lens<BattleModel, World>, screen : Screen, _ : World) =
            Content.layers (model --> fun model -> getAllies model) $ fun index ally _ ->
                let allyIndex = AllyIndex index
                let input = screen / ("Input" + "+" + scstring index)
                Content.layer input.Name []
                    [Content.entity<RingMenuDispatcher> "RegularMenu"
                        [Entity.Position <== ally --> fun ally -> ally.Center
                         Entity.Depth == 10.0f
                         Entity.Visible <== ally --> fun ally -> ally.InputState = RegularMenu
                         Entity.RingMenuModel == { Items = ["Attack"; "Defend"; "Special"; "Item"]; ItemCancelOpt = None }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (RegularItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (RegularItemCancel allyIndex)]
                     Content.entity<RingMenuDispatcher> "SpecialMenu"
                        [Entity.Position <== ally --> fun ally -> ally.Center
                         Entity.Depth == 10.0f
                         Entity.Visible <== ally --> fun ally -> ally.InputState = SpecialMenu
                         Entity.RingMenuModel <== ally --> fun ally ->
                             let specials = List.ofSeq ally.CharacterState.Specials
                             let specialStrs = List.map scstring specials
                             { Items = specialStrs; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (SpecialItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (SpecialItemCancel allyIndex)]
                     Content.entity<RingMenuDispatcher> "ItemMenu"
                        [Entity.Position <== ally --> fun ally -> ally.Center
                         Entity.Depth == 10.0f
                         Entity.Visible <== ally --> fun ally -> ally.InputState = ItemMenu
                         Entity.RingMenuModel <== model --> fun model ->
                            let consumables = Inventory.getConsumables model.Inventory
                            let items = Map.toKeyList consumables
                            let itemStrs = List.map scstring items
                            { Items = itemStrs; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (ItemItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (ItemItemCancel allyIndex)]
                     Content.entity<ReticlesDispatcher> "Reticles"
                        [Entity.Position <== ally --> fun ally -> ally.Center
                         Entity.Depth == 10.0f
                         Entity.Visible <== ally --> fun ally -> match ally.InputState with AimReticles _ -> true | _ -> false
                         Entity.ReticlesModel <== model --> fun model -> { Characters = model.Characters; AimType = (getCharacter allyIndex model).InputState.AimType }
                         Entity.TargetSelectEvent ==|> fun evt -> msg (ReticlesSelect (evt.Data, allyIndex))
                         Entity.CancelEvent ==> msg (ReticlesCancel allyIndex)]]

        override this.Content (model, screen, world) =
            [this.SceneContent (model, screen, world)
             this.InputContent (model, screen, world)]