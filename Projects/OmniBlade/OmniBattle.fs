namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniBattle =

    type [<NoEquality; NoComparison>] ActionCommand =
        { Action : ActionType
          Source : CharacterIndex
          TargetOpt : CharacterIndex option }

        static member make action source targetOpt =
            { Action = action
              Source = source
              TargetOpt = targetOpt }
              
    type [<NoEquality; NoComparison>] CurrentCommand =
        { TimeStart : int64
          ActionCommand : ActionCommand }

        static member make timeStart actionCommand =
            { TimeStart = timeStart; ActionCommand = actionCommand }

    type [<NoEquality; NoComparison>] BattleRunning =
        { CurrentCommandOpt : CurrentCommand option
          ActionQueue : ActionCommand Queue }

        static member make () =
            { CurrentCommandOpt = None
              ActionQueue = Queue.empty }

    type [<NoEquality; NoComparison>] BattleState =
        | BattleReady of int64
        | BattleRunning of BattleRunning
        | BattleCease of bool * int64

    type [<NoEquality; NoComparison>] BattleModel =
        { BattleCharacters : Map<CharacterIndex, CharacterModel>
          BattleState : BattleState }

    and [<NoComparison>] BattleMessage =
        | ReadyCharactersM
        | PoiseCharactersM
        | CelebrateCharactersM of bool
        | AdvanceCharactersM
        | AttackCharacterM of CharacterIndex * CharacterIndex
        | ResetCharacterM of CharacterIndex
        | DamageCharacterM of CharacterIndex
        | PoiseCharacterM of CharacterIndex
        | WoundCharacterM of CharacterIndex
        | DestroyCharacterM of CharacterIndex
        | ReticlesSelect of Entity * CharacterIndex
        | Tick

    and [<NoComparison>] IndexedCommand =
        | RegularMenuShow
        | RegularMenuSelect of string
        | SpecialMenuSelect of string
        | ItemMenuSelect of string
        | ReticlesCancel

    and [<NoComparison>] BattleCommand =
        | InitializeBattle
        | FinalizeBattle
        | DestroyCharacter of CharacterIndex
        | ResetCharacter of CharacterIndex
        | IndexedCommand of IndexedCommand * int
        | FadeSong

    and Screen with

        member this.GetBattleState = this.GetModel<BattleState>
        member this.SetBattleState = this.SetModel<BattleState>
        member this.BattleState = this.Model<BattleState>

    and BattleDispatcher () =
        inherit ScreenDispatcher<BattleModel, BattleMessage, BattleCommand> (
            let allies =
                [{ CharacterPosition = v2 -224.0f -168.0f
                   CharacterSize = v2 160.0f 160.0f
                   CharacterAnimationSheet = asset "Battle" "Jinn"
                   CharacterAnimationState = { TimeStart = 0L; CharacterAnimationCycle = ReadyCycle; Direction = Rightward; Stutter = 10 }
                   CharacterState = { CharacterType = Ally Jinn; PartyIndex = 0; ExpPoints = 0; HitPoints = 12; SpecialPoints = 1; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Statuses = Set.empty; WeaponOpt = Some "Wooden Sword"; ArmorOpt = None; Relics = [] }
                   ActionTime = 0 }] |>
                List.mapi (fun i ally -> (AllyIndex i, ally))
            let enemies =
                [{ CharacterPosition = v2 0.0f 64.0f
                   CharacterSize = v2 160.0f 160.0f
                   CharacterAnimationSheet = asset "Battle" "Goblin"
                   CharacterAnimationState = { TimeStart = 0L; CharacterAnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Relics = [] }
                   ActionTime = 0 }
                 { CharacterPosition = v2 176.0f -152.0f
                   CharacterSize = v2 160.0f 160.0f
                   CharacterAnimationSheet = asset "Battle" "Goblin"
                   CharacterAnimationState = { TimeStart = 0L; CharacterAnimationCycle = ReadyCycle; Direction = Leftward; Stutter = 10 }
                   CharacterState = { CharacterType = Enemy Goblin; PartyIndex = 0; ExpPoints = 0; HitPoints = 5; SpecialPoints = 1; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Statuses = Set.empty; WeaponOpt = Some "Melee"; ArmorOpt = None; Relics = [] }
                   ActionTime = 0 }] |>
                List.mapi (fun i ally -> (EnemyIndex i, ally))
            let characters =
                Map.ofList (allies @ enemies)
            { BattleCharacters = characters
              BattleState = BattleReady 0L })

        static member getAllies model =
            model.BattleCharacters |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

        static member getEnemies model =
            model.BattleCharacters |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

        static member updateCharacters3 predicate updater model =
            { model with BattleCharacters = Map.map (fun index character -> if predicate index then updater character else character) model.BattleCharacters }

        static member updateCharacters updater model =
            BattleDispatcher.updateCharacters3 tautology updater model

        static member updateAllies updater model =
            BattleDispatcher.updateCharacters3 (function AllyIndex _ -> true | _ -> false) updater model

        static member updateEnemies updater model =
            BattleDispatcher.updateCharacters3 (function EnemyIndex _ -> true | _ -> false) updater model

        static member tryGetCharacter characterIndex model =
            Map.tryFind characterIndex model.BattleCharacters

        static member getCharacter characterIndex model =
            BattleDispatcher.tryGetCharacter characterIndex model |> Option.get

        static member getCharacterIndex (entity : Entity) =
            let strs = entity.Name.Split '+'
            let ctor = if strs.[0] = "Ally" then AllyIndex else EnemyIndex
            ctor (scvalue<int> strs.[1])

        static member tryUpdateCharacter updater characterIndex model =
            match BattleDispatcher.tryGetCharacter characterIndex model with
            | Some character ->
                let character = updater character
                { model with BattleCharacters = Map.add characterIndex character model.BattleCharacters }
            | None -> model

        static member updateCharacter updater characterIndex model =
            let character = BattleDispatcher.getCharacter characterIndex model
            let character = updater character
            { model with BattleCharacters = Map.add characterIndex character model.BattleCharacters }

        static member tickAttack sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal battleRunning model =

            // if target available, proceed with attack
            let source = BattleDispatcher.getCharacter sourceIndex model
            match targetIndexOpt with
            | Some targetIndex ->
                let target = BattleDispatcher.getCharacter targetIndex model
                match timeLocal with
                | 0L when target.CharacterState.IsHealthy ->
                    if target.CharacterState.IsHealthy then
                        withMsg battleRunning (AttackCharacterM (sourceIndex, targetIndex))
                    else // target wounded, cancel attack
                        let battleRunning = { battleRunning with CurrentCommandOpt = None }
                        withMsgs battleRunning [ResetCharacterM sourceIndex; PoiseCharacterM sourceIndex]
                | _ when timeLocal = 1L * int64 source.CharacterAnimationState.Stutter ->
                    withMsg battleRunning (DamageCharacterM targetIndex)
                | _ ->
                    if CharacterAnimationState.finished time source.CharacterAnimationState then
                        if target.CharacterState.IsHealthy then
                            let battleRunning = { battleRunning with CurrentCommandOpt = None }
                            withMsgs battleRunning [PoiseCharacterM sourceIndex; PoiseCharacterM targetIndex]
                        else
                            let woundCommand = CurrentCommand.make time (ActionCommand.make Wound targetIndex None)
                            let battleRunning = { battleRunning with CurrentCommandOpt = Some woundCommand }
                            withMsg battleRunning (PoiseCharacterM sourceIndex)
                    else just battleRunning

            // else target destroyed, cancel attack
            | None ->
                let battleRunning = { battleRunning with CurrentCommandOpt = None }
                withMsgs battleRunning [ResetCharacterM sourceIndex; PoiseCharacterM sourceIndex]

        static member tickWound characterIndex time timeLocal battleRunning model =
            match timeLocal with
            | 0L ->
                withMsg battleRunning (DamageCharacterM characterIndex)
            | _ ->
                let character = BattleDispatcher.getCharacter characterIndex model
                if character.CharacterState.IsAlly then
                    match character.CharacterAnimationState.CharacterAnimationCycle with
                    | DamageCycle ->
                        if CharacterAnimationState.finished time character.CharacterAnimationState
                        then withMsg { battleRunning with CurrentCommandOpt = None } (WoundCharacterM characterIndex)
                        else just battleRunning
                    | _ -> failwithumf ()
                else
                    match character.CharacterAnimationState.CharacterAnimationCycle with
                    | DamageCycle ->
                        if CharacterAnimationState.finished time character.CharacterAnimationState
                        then withMsg battleRunning (WoundCharacterM characterIndex)
                        else just battleRunning
                    | WoundCycle ->
                        if CharacterAnimationState.finished time character.CharacterAnimationState
                        then withMsg { battleRunning with CurrentCommandOpt = None } (DestroyCharacterM characterIndex)
                        else just battleRunning
                    | _ -> failwithumf ()

        static member tickReady time timeStart (model : BattleModel) =
            let timeLocal = time - timeStart
            match timeLocal with
            | 0L -> withMsg { model with BattleState = BattleRunning (BattleRunning.make ()) } ReadyCharactersM
            | 30L -> withMsg { model with BattleState = BattleRunning (BattleRunning.make ()) } PoiseCharactersM
            | _ -> just model

        static member tickCurrentCommand time currentCommand battleRunning (model : BattleModel) =
            let timeLocal = time - currentCommand.TimeStart
            match currentCommand.ActionCommand.Action with
            | Attack ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                let (battleRunning, signals) = BattleDispatcher.tickAttack source targetOpt time timeLocal battleRunning model
                ({ model with BattleState = BattleRunning battleRunning}, signals)
            | Defend -> just { model with BattleState = BattleRunning battleRunning }
            | Consume _ -> just { model with BattleState = BattleRunning battleRunning }
            | Special _ -> just { model with BattleState = BattleRunning battleRunning }
            | Wound ->
                let source = currentCommand.ActionCommand.Source
                let (battleRunning, signal) = BattleDispatcher.tickWound source time timeLocal battleRunning model
                match battleRunning.CurrentCommandOpt with
                | Some _ ->
                    // keep ticking wound
                    let model = { model with BattleState = BattleRunning battleRunning }
                    withSig model signal
                | None ->
                    let (model, signal2) =
                        let allies = BattleDispatcher.getAllies model
                        let enemies = BattleDispatcher.getEnemies model
                        if Seq.forall (fun character -> character.CharacterState.IsWounded) allies
                        then BattleDispatcher.tick time { model with BattleState = BattleCease (false, time) } // tick for frame 0
                        elif Seq.forall (fun character -> character.CharacterState.IsWounded) enemies
                        then BattleDispatcher.tick time { model with BattleState = BattleCease (true, time) } // tick for frame 0
                        else just { model with BattleState = BattleRunning battleRunning }
                    withSig model (signal + signal2)

        static member tickNoCommand time battleRunning (model : BattleModel) =
            match battleRunning.ActionQueue with
            | Queue.Cons (currentCommand, nextCommands) ->
                let command = CurrentCommand.make time currentCommand
                let model = { model with BattleState = BattleRunning { battleRunning with CurrentCommandOpt = Some command; ActionQueue = nextCommands }}
                BattleDispatcher.tick time model // tick for frame 0
            | Queue.Nil ->
                let (allySignals, model) =
                    List.fold (fun (commands, model) ally ->
                        if ally.ActionTime = Constants.Battle.ActionTime
                        then (Command (IndexedCommand (RegularMenuShow, ally.CharacterState.PartyIndex)) :: commands, model)
                        else (commands, model))
                        ([], model)
                        (BattleDispatcher.getAllies model)
                let (enemySignals, model) =
                    List.fold (fun (commands, model) enemy ->
                        let enemyIndex = EnemyIndex enemy.CharacterState.PartyIndex
                        let allies = BattleDispatcher.getAllies model
                        let allyIndex = (Random ()).Next allies.Length
                        let attack = { Action = Attack; Source = enemyIndex; TargetOpt = Some (AllyIndex allyIndex) }
                        if enemy.ActionTime = Constants.Battle.ActionTime then
                            let model = { model with BattleState = BattleRunning { battleRunning with ActionQueue = Queue.conj attack battleRunning.ActionQueue }}
                            (Message (ResetCharacterM enemyIndex) :: commands, model)
                        else (commands, model))
                        ([], model)
                        (BattleDispatcher.getEnemies model)
                let advanceCharactersSignal = Message AdvanceCharactersM
                let signals = advanceCharactersSignal :: enemySignals @ allySignals
                withSigs model signals

        static member tickRunning time battleRunning model =
            match battleRunning.CurrentCommandOpt with
            | Some currentCommand -> BattleDispatcher.tickCurrentCommand time currentCommand battleRunning model
            | None -> BattleDispatcher.tickNoCommand time battleRunning model

        static member tickCease time timeStart outcome (model : BattleModel) =
            let timeLocal = time - timeStart
            match timeLocal with
            | 0L -> withMsg model (CelebrateCharactersM outcome)
            | _ -> just model

        static member tick time (model : BattleModel) =
            let (model, sigs) =
                match model.BattleState with
                | BattleReady timeStart -> BattleDispatcher.tickReady time timeStart model
                | BattleRunning battleRunning -> BattleDispatcher.tickRunning time battleRunning model
                | BattleCease (outcome, timeStart) -> BattleDispatcher.tickCease time timeStart outcome model
            (model, sigs)

        static member tickWorld (model : BattleModel) world =
            BattleDispatcher.tick (World.getTickTime world) model

        static member inputBindings index =
            let regularMenu = Simulants.RegularMenu index
            let specialMenu = Simulants.SpecialMenu index
            let itemMenu = Simulants.ItemMenu index
            let reticles = Simulants.Reticles index
            [regularMenu.ItemSelectEvent =|>! fun evt -> IndexedCommand (RegularMenuSelect evt.Data, index)
             specialMenu.ItemSelectEvent =|>! fun evt -> IndexedCommand (SpecialMenuSelect evt.Data, index)
             specialMenu.CancelEvent =>! IndexedCommand (RegularMenuShow, index)
             itemMenu.ItemSelectEvent =|>! fun evt -> IndexedCommand (ItemMenuSelect evt.Data, index)
             itemMenu.CancelEvent =>! IndexedCommand (RegularMenuShow, index)
             reticles.TargetSelectEvent =|> fun evt -> ReticlesSelect (evt.Data, AllyIndex index)
             reticles.CancelEvent =>! IndexedCommand (ReticlesCancel, index)]

        static member inputContent index =
            let input = Simulants.Input index
            let regularMenu = Simulants.RegularMenu index
            let specialMenu = Simulants.SpecialMenu index
            let itemMenu = Simulants.ItemMenu index
            let reticles = Simulants.Reticles index
            Content.layer input.Name
                [Entity.Depth == 10.0f]
                [Content.entity<RingMenuDispatcher> regularMenu.Name
                    [Entity.Visible == false
                     Entity.Items == ["Attack"; "Defend"; "Special"; "Item"]]
                 Content.entity<RingMenuDispatcher> specialMenu.Name
                    [Entity.Visible == false
                     Entity.Items == ["Attack"]
                     Entity.ItemCancelOpt == Some "Cancel"]
                 Content.entity<RingMenuDispatcher> itemMenu.Name
                    [Entity.Visible == false
                     Entity.Items == ["Attack"]
                     Entity.ItemCancelOpt == Some "Cancel"]
                 Content.entity<ReticlesDispatcher> reticles.Name
                    [reticles.Visible == false]]

        override this.Bindings (_, battle, _) =
            [battle.SelectEvent =>! InitializeBattle
             battle.DeselectEvent =>! FinalizeBattle
             battle.OutgoingStartEvent =>! FadeSong
             battle.UpdateEvent => Tick] @
             BattleDispatcher.inputBindings 0 @
             BattleDispatcher.inputBindings 1 @
             BattleDispatcher.inputBindings 2

        override this.Message (message, model, _, world) =
            match message with
            | ReadyCharactersM ->
                let time = World.getTickTime world
                let model = BattleDispatcher.updateCharacters (fun character -> { character with CharacterAnimationState = (CharacterAnimationState.setCycle (Some time) ReadyCycle) character.CharacterAnimationState }) model
                just model
            | PoiseCharactersM ->
                let time = World.getTickTime world
                let model = BattleDispatcher.updateCharacters (fun character -> { character with CharacterAnimationState = (CharacterAnimationState.setCycle (Some time) PoiseCycle) character.CharacterAnimationState }) model
                just model
            | CelebrateCharactersM outcome ->
                let time = World.getTickTime world
                let model =
                    if outcome
                    then BattleDispatcher.updateAllies (fun character -> { character with CharacterAnimationState = (CharacterAnimationState.setCycle (Some time) CelebrateCycle) character.CharacterAnimationState }) model
                    else BattleDispatcher.updateAllies (fun character -> { character with CharacterAnimationState = (CharacterAnimationState.setCycle (Some time) CelebrateCycle) character.CharacterAnimationState }) model
                just model
            | AdvanceCharactersM ->
                let model = BattleDispatcher.updateCharacters (fun character -> { character with ActionTime = character.ActionTime + Constants.Battle.ActionTimeInc }) model
                just model
            | AttackCharacterM (sourceIndex, targetIndex) ->
                let time = World.getTickTime world
                let source = BattleDispatcher.getCharacter sourceIndex model
                let target = BattleDispatcher.getCharacter targetIndex model
                let model = BattleDispatcher.updateCharacter (fun character -> { character with CharacterAnimationState = (CharacterAnimationState.setCycle (Some time) AttackCycle) character.CharacterAnimationState }) sourceIndex model
                let model =
                    BattleDispatcher.updateCharacter (fun character ->
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
                then withSigs model [Message (ResetCharacterM targetIndex); Command (ResetCharacter targetIndex)]
                else just model
            | ResetCharacterM characterIndex ->
                let model = BattleDispatcher.updateCharacter (fun character -> { character with ActionTime = 0 }) characterIndex model
                withCmd model (ResetCharacter characterIndex)
            | DamageCharacterM characterIndex ->
                let time = World.getTickTime world
                let model = BattleDispatcher.updateCharacter (fun character -> { character with CharacterAnimationState = (CharacterAnimationState.setCycle (Some time) DamageCycle) character.CharacterAnimationState }) characterIndex model
                just model
            | PoiseCharacterM characterIndex ->
                let time = World.getTickTime world
                let model = BattleDispatcher.updateCharacter (fun character -> { character with CharacterAnimationState = (CharacterAnimationState.setCycle (Some time) PoiseCycle) character.CharacterAnimationState }) characterIndex model
                just model
            | WoundCharacterM characterIndex ->
                let time = World.getTickTime world
                let model = BattleDispatcher.updateCharacter (fun character -> { character with CharacterAnimationState = (CharacterAnimationState.setCycle (Some time) WoundCycle) character.CharacterAnimationState }) characterIndex model
                just model
            | DestroyCharacterM characterIndex ->
                withCmd model (DestroyCharacter characterIndex)
            | ReticlesSelect (targetEntity, allyIndex) ->
                match model.BattleState with
                | BattleRunning battleRunning ->
                    let targetIndex = BattleDispatcher.getCharacterIndex targetEntity
                    let command = ActionCommand.make Attack allyIndex (Some targetIndex)
                    let model = { model with BattleState = BattleRunning { battleRunning with ActionQueue = Queue.conj command battleRunning.ActionQueue }}
                    withMsg model (ResetCharacterM allyIndex)
                | _ -> just model
            | Tick ->
                if World.isTicking world
                then BattleDispatcher.tickWorld model world
                else just model

        override this.Command (command, model, battle, world) =
            match command with
            | InitializeBattle ->
                let world = World.hintRenderPackageUse Assets.BattlePackage world
                let world = World.hintAudioPackageUse Assets.BattlePackage world
                let world = World.playSong 0 (1.0f * Constants.Audio.MasterSongVolume) Assets.BattleSong world
                just (battle.SetBattleState (BattleReady (World.getTickTime world)) world)
            | FinalizeBattle ->
                let world = World.hintRenderPackageDisuse Assets.BattlePackage world
                just (World.hintAudioPackageDisuse Assets.BattlePackage world)
            | ResetCharacter characterIndex ->
                let character = BattleDispatcher.getCharacter characterIndex model
                let world =
                    if character.CharacterState.IsAlly then
                        let index = character.CharacterState.PartyIndex
                        let entities = Simulants.AllInputEntities index
                        List.fold (fun world (entity : Entity) -> entity.SetVisible false world) world entities
                    else world
                just world
            | DestroyCharacter characterIndex ->
                let character = Simulants.Character characterIndex
                just (World.destroyEntity character world)
            | IndexedCommand (command, index) ->
                this.IndexedCommand (command, index, battle, world)
            | FadeSong ->
                just (World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world)

        member this.IndexedCommand (command, index, _, world) =
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

        override this.Content (model, _, _) =
            [BattleDispatcher.inputContent 0
             BattleDispatcher.inputContent 1
             BattleDispatcher.inputContent 2
             Content.layer Simulants.Scene.Name []
                [Content.label "Background"
                    [Entity.Position == v2 -480.0f -512.0f
                     Entity.Size == v2 1024.0f 1024.0f
                     Entity.Depth == -10.0f
                     Entity.LabelImage == asset "Battle" "Background"]
                 Content.entitiesi (model.MapOut (fun model -> seq (BattleDispatcher.getAllies model))) $ fun i model _ _ ->
                    Content.entity<CharacterDispatcher> ("Ally+" + scstring i)
                        [Entity.Position ==> model.MapOut (fun model -> model.CharacterPosition)
                         Entity.Size ==> model.MapOut (fun model -> model.CharacterSize)
                         Entity.CharacterAnimationSheet ==> model.MapOut (fun model -> model.CharacterAnimationSheet)
                         Entity.CharacterAnimationState ==> model.MapOut (fun model -> model.CharacterAnimationState)
                         Entity.CharacterState ==> model.MapOut (fun model -> model.CharacterState)]]]