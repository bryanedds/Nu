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
        | ReadyCharacters
        | PoiseCharacters
        | CelebrateCharacters of bool
        | AdvanceCharacters
        | AttackCharacter of CharacterIndex * CharacterIndex
        | DamageCharacter of CharacterIndex
        | PoiseCharacter of CharacterIndex
        | WoundCharacter of CharacterIndex
        | ResetCharacter of CharacterIndex
        | DestroyCharacter of CharacterIndex
        | ReticlesSelect of CharacterIndex * CharacterIndex
        | Tick

    and [<NoComparison>] IndexedCommand =
        | RegularMenuShow
        | RegularMenuSelect of string
        | SpecialMenuSelect of string
        | ItemMenuSelect of string
        | ReticlesCancel
        | AllyInputHide

    and [<NoComparison>] BattleCommand =
        | FadeSong
        | InitializeBattle
        | FinalizeBattle
        | IndexedCommand of IndexedCommand * int

    and Screen with

        member this.GetBattleModel = this.GetModel<BattleModel>
        member this.SetBattleModel = this.SetModel<BattleModel>
        member this.BattleModel = this.Model<BattleModel> ()

    and BattleDispatcher () =
        inherit ScreenDispatcher<BattleModel, BattleMessage, BattleCommand>
            (let allies =
                [{ CharacterState = { CharacterType = Ally Jinn; PartyIndex = 0; ActionTime = 600; ExpPoints = 0; HitPoints = 20; SpecialPoints = 1; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Statuses = Set.empty; WeaponOpt = Some "Wooden Sword"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Jinn"; AnimationCycle = ReadyCycle; Direction = Rightward; Stutter = 10 }
                   InputState = NoInput
                   Position = v2 -224.0f -168.0f
                   Size = v2 160.0f 160.0f }]
             let enemies =
                [{ CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 0; ActionTime = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Relics = [] }
                   AnimationState = { TimeStart = 0L; AnimationSheet = asset "Battle" "Goblin"; AnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   InputState = NoInput
                   Position = v2 0.0f 64.0f
                   Size = v2 160.0f 160.0f }
                 { CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 1; ActionTime = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Relics = [] }
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
               AimType = NoAim })

        static let getAllies model =
            CharacterModels.getAllies model.Characters

        static let getEnemies model =
            CharacterModels.getEnemies model.Characters

        static let updateCharacters3 predicate updater model =
            { model with BattleModel.Characters = Map.map (fun index character -> if predicate index then updater character else character) model.Characters }

        static let updateCharacters updater model =
            updateCharacters3 tautology updater model

        static let updateAllies updater model =
            updateCharacters3 (function AllyIndex _ -> true | _ -> false) updater model

        static let updateEnemies updater model =
            updateCharacters3 (function EnemyIndex _ -> true | _ -> false) updater model

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

        static let tickAttack sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            let source = getCharacter sourceIndex model
            match targetIndexOpt with
            | Some targetIndex ->
                let target = getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.CharacterState.IsHealthy
                    then withMsg model (AttackCharacter (sourceIndex, targetIndex))
                    else withMsgs { model with CurrentCommandOpt = None } [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | _ ->
                    if timeLocal = 1L * int64 source.AnimationState.Stutter
                    then withMsg model (DamageCharacter targetIndex)
                    elif CharacterAnimationState.finished time source.AnimationState then
                        let target = getCharacter targetIndex model
                        if target.CharacterState.IsHealthy then
                            let model = { model with CurrentCommandOpt = None }
                            withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                        else
                            let woundCommand = CurrentCommand.make time (ActionCommand.make Wound targetIndex None)
                            let model = { model with CurrentCommandOpt = Some woundCommand }
                            withMsg model (PoiseCharacter sourceIndex)
                    else just model
            | None ->
                let model = { model with CurrentCommandOpt = None }
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickWound characterIndex time timeLocal model =
            match timeLocal with
            | 0L ->
                withMsg model (DamageCharacter characterIndex)
            | _ ->
                let character = getCharacter characterIndex model
                if character.CharacterState.IsAlly then
                    match character.AnimationState.AnimationCycle with
                    | DamageCycle ->
                        if CharacterAnimationState.finished time character.AnimationState
                        then withMsg { model with CurrentCommandOpt = None } (WoundCharacter characterIndex)
                        else just model
                    | _ -> failwithumf ()
                else
                    match character.AnimationState.AnimationCycle with
                    | DamageCycle ->
                        if CharacterAnimationState.finished time character.AnimationState
                        then withMsg model (WoundCharacter characterIndex)
                        else just model
                    | WoundCycle ->
                        if CharacterAnimationState.finished time character.AnimationState
                        then withMsg { model with CurrentCommandOpt = None } (DestroyCharacter characterIndex)
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
            | Defend -> just model // TODO: make the act of defending grant a significant counter buff
            | Consume _ -> just model
            | Special _ -> just model
            | Wound ->
                let source = currentCommand.ActionCommand.Source
                let (model, signal) = tickWound source time timeLocal model
                match model.CurrentCommandOpt with
                | Some _ -> withSig model signal // keep ticking wound
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

        and tickNoCommand time model =
            match model.ActionQueue with
            | Queue.Cons (currentCommand, nextCommands) ->
                let command = CurrentCommand.make time currentCommand
                let model = { model with CurrentCommandOpt = Some command; ActionQueue = nextCommands }
                tick time model // tick for frame 0
            | Queue.Nil ->
                let (allySignalsRev, model) =
                    List.fold (fun (commands, model) ally ->
                        if ally.CharacterState.ActionTime = Constants.Battle.ActionTime
                        then (Command (IndexedCommand (RegularMenuShow, ally.CharacterState.PartyIndex)) :: commands, model)
                        else (commands, model))
                        ([], model)
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
                let signals = advanceCharactersSignal :: List.rev enemySignalsRev @ List.rev allySignalsRev
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

        static let battleBindings (battle : Screen) =
            [battle.OutgoingStartEvent => cmd FadeSong
             battle.SelectEvent => cmd InitializeBattle
             battle.DeselectEvent => cmd FinalizeBattle
             battle.UpdateEvent => msg Tick]

        static let allyBindings index =
            let regularMenu = Simulants.RegularMenu index
            let specialMenu = Simulants.SpecialMenu index
            let itemMenu = Simulants.ItemMenu index
            let reticles = Simulants.Reticles index
            [regularMenu.ItemSelectEvent =|> fun evt -> cmd (IndexedCommand (RegularMenuSelect evt.Data, index))
             specialMenu.ItemSelectEvent =|> fun evt -> cmd (IndexedCommand (SpecialMenuSelect evt.Data, index))
             specialMenu.CancelEvent => cmd (IndexedCommand (RegularMenuShow, index))
             itemMenu.ItemSelectEvent =|> fun evt -> cmd (IndexedCommand (ItemMenuSelect evt.Data, index))
             itemMenu.CancelEvent => cmd (IndexedCommand (RegularMenuShow, index))
             reticles.TargetSelectEvent =|> fun evt -> msg (ReticlesSelect (evt.Data, AllyIndex index))
             reticles.CancelEvent => cmd (IndexedCommand (ReticlesCancel, index))]

        static let allyPartyBindings allyMax =
            seq { for i in 0 .. allyMax - 1 do yield allyBindings i } |>
            Seq.concat |>
            Seq.toList

        static let indexedCommand command index world =
            let ally = Simulants.Ally index
            let regularMenu = Simulants.RegularMenu index
            let specialMenu = Simulants.SpecialMenu index
            let itemMenu = Simulants.ItemMenu index
            let reticles = Simulants.Reticles index
            match command with
            | RegularMenuShow ->
                let world = specialMenu.SetVisible false world
                let world = itemMenu.SetVisible false world
                let world = reticles.SetVisible false world
                let world = regularMenu.SetVisible true world
                just (regularMenu.SetCenter (ally.GetCenter world) world)
            | RegularMenuSelect item ->
                match item with
                | "Attack" ->
                    let currentMenu =
                        if regularMenu.GetVisible world then regularMenu
                        elif specialMenu.GetVisible world then specialMenu
                        else itemMenu
                    let world = currentMenu.SetVisible false world
                    let world = reticles.SetVisible true world
                    let world = reticles.UpdateModel (fun model -> { model with BattleModel.AimType = EnemyAim }) world
                    just (reticles.AttachProperty Property? PreviousMenu false true { PropertyType = typeof<Entity>; PropertyValue = currentMenu } world)
                | "Defend" ->
                    just world
                | "Special" ->
                    let world = regularMenu.SetVisible false world
                    let world = specialMenu.SetVisible true world
                    just (specialMenu.SetCenter (ally.GetCenter world) world)
                | "Item" ->
                    let world = regularMenu.SetVisible false world
                    let world = itemMenu.SetVisible true world
                    just (itemMenu.SetCenter (ally.GetCenter world) world)
                | _ -> failwithumf ()
            | SpecialMenuSelect _ ->
                just world
            | ItemMenuSelect _ ->
                just world
            | ReticlesCancel ->
                let previousMenu = (reticles.GetProperty Property? PreviousMenu world).PropertyValue :?> Entity
                let world = reticles.SetVisible false world
                let world = previousMenu.SetVisible true world
                just (previousMenu.SetCenter (ally.GetCenter world) world)
            | AllyInputHide ->
                let world =
                    let entities = Simulants.AllInputEntities index
                    List.fold (fun world (entity : Entity) -> entity.SetVisible false world) world entities
                just world

        override this.Bindings (_, battle, _) =
            battleBindings battle @
            allyPartyBindings Constants.Battle.AllyMax

        override this.Message (model, message, _, world) =
            match message with
            | ReadyCharacters ->
                let time = World.getTickTime world
                let model = updateCharacters (fun character -> { character with AnimationState = (CharacterAnimationState.setCycle (Some time) ReadyCycle) character.AnimationState }) model
                just model
            | PoiseCharacters ->
                let time = World.getTickTime world
                let model = updateCharacters (fun character -> { character with AnimationState = (CharacterAnimationState.setCycle (Some time) PoiseCycle) character.AnimationState }) model
                just model
            | CelebrateCharacters outcome ->
                let time = World.getTickTime world
                let model =
                    if outcome
                    then updateAllies (fun character -> { character with AnimationState = (CharacterAnimationState.setCycle (Some time) CelebrateCycle) character.AnimationState }) model
                    else updateEnemies (fun character -> { character with AnimationState = (CharacterAnimationState.setCycle (Some time) CelebrateCycle) character.AnimationState }) model
                just model
            | AdvanceCharacters ->
                let model = updateCharacters (fun character -> { character with CharacterState = { character.CharacterState with ActionTime = character.CharacterState.ActionTime + Constants.Battle.ActionTimeInc }}) model
                just model
            | AttackCharacter (sourceIndex, targetIndex) ->
                let time = World.getTickTime world
                let source = getCharacter sourceIndex model
                let target = getCharacter targetIndex model
                let model = updateCharacter (fun character -> { character with AnimationState = (CharacterAnimationState.setCycle (Some time) AttackCycle) character.AnimationState }) sourceIndex model
                let model =
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
                if target.CharacterState.HitPoints = 0 && target.CharacterState.IsAlly
                then withSig model (Message (ResetCharacter targetIndex))
                else just model
            | DamageCharacter characterIndex ->
                let time = World.getTickTime world
                let model = updateCharacter (fun character -> { character with AnimationState = (CharacterAnimationState.setCycle (Some time) DamageCycle) character.AnimationState }) characterIndex model
                just model
            | PoiseCharacter characterIndex ->
                let time = World.getTickTime world
                let model = updateCharacter (fun character -> { character with AnimationState = (CharacterAnimationState.setCycle (Some time) PoiseCycle) character.AnimationState }) characterIndex model
                just model
            | WoundCharacter characterIndex ->
                let time = World.getTickTime world
                let model = updateCharacter (fun character -> { character with AnimationState = (CharacterAnimationState.setCycle (Some time) WoundCycle) character.AnimationState }) characterIndex model
                just model
            | ResetCharacter characterIndex ->
                let model = updateCharacter (fun character -> { character with CharacterState = { character.CharacterState with ActionTime = 0 }}) characterIndex model
                let character = getCharacter characterIndex model
                if character.CharacterState.IsAlly
                then withCmd model (IndexedCommand (AllyInputHide, character.CharacterState.PartyIndex))
                else just model
            | DestroyCharacter characterIndex ->
                let character = getCharacter characterIndex model
                let model =
                    if character.CharacterState.IsEnemy
                    then { model with Characters = Map.remove characterIndex model.Characters }
                    else model
                just model
            | ReticlesSelect (targetIndex, allyIndex) ->
                match model.BattleState with
                | BattleRunning ->
                    let command = ActionCommand.make Attack allyIndex (Some targetIndex)
                    let model = { model with ActionQueue = Queue.conj command model.ActionQueue }
                    withMsg model (ResetCharacter allyIndex)
                | _ -> just model
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
            | IndexedCommand (command, index) ->
                indexedCommand command index world

        override this.Content (model, _, _) =
            [Content.layer Simulants.Scene.Name []
                [Content.label "Background"
                    [Entity.Position == v2 -480.0f -512.0f
                     Entity.Size == v2 1024.0f 1024.0f
                     Entity.Depth == -10.0f
                     Entity.LabelImage == asset "Battle" "Background"]
                 Content.entitiesIndexed
                    (model --> fun model -> getAllies model)
                    (fun model -> model.CharacterState.PartyIndex)
                    (fun allyIndex ally _ _ ->
                        Content.entity<CharacterDispatcher> ("Ally+" + scstring allyIndex)
                            [Entity.CharacterState <== ally --> fun ally -> ally.CharacterState
                             Entity.CharacterAnimationState <== ally --> fun ally -> ally.AnimationState
                             Entity.Position <== ally --> fun ally -> ally.Position
                             Entity.Size <== ally --> fun ally -> ally.Size])
                 Content.entitiesIndexed
                    (model --> fun model -> getEnemies model)
                    (fun model -> model.CharacterState.PartyIndex)
                    (fun enemyIndex enemy _ _ ->
                        Content.entity<CharacterDispatcher> ("Enemy+" + scstring enemyIndex)
                            [Entity.CharacterState <== enemy --> fun enemy -> enemy.CharacterState
                             Entity.CharacterAnimationState <== enemy --> fun enemy -> enemy.AnimationState
                             Entity.Position <== enemy --> fun enemy -> enemy.Position
                             Entity.Size <== enemy --> fun enemy -> enemy.Size])]
             Content.layers (model --> fun model -> getAllies model) $ fun allyIndex _ _ _ ->
                Content.layer (Simulants.Input allyIndex).Name []
                    [Content.entity<RingMenuDispatcher> (Simulants.RegularMenu allyIndex).Name
                        [Entity.RingMenuModel == { Items = ["Attack"; "Defend"; "Special"; "Item"]; ItemCancelOpt = None }
                         Entity.Depth == 10.0f]
                     Content.entity<RingMenuDispatcher> (Simulants.SpecialMenu allyIndex).Name
                        [Entity.RingMenuModel == { Items = ["Attack"]; ItemCancelOpt = Some "Cancel" }
                         Entity.Depth == 10.0f]
                     Content.entity<RingMenuDispatcher> (Simulants.ItemMenu allyIndex).Name
                        [Entity.RingMenuModel == { Items = ["Attack"]; ItemCancelOpt = Some "Cancel" }
                         Entity.Depth == 10.0f]
                     Content.entity<ReticlesDispatcher> (Simulants.Reticles allyIndex).Name
                        [Entity.ReticlesModel <== model --> fun model -> { Characters = model.Characters; AimType = model.AimType }
                         Entity.Depth == 10.0f]]]