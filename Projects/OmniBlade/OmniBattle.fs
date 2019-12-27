namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniBattle =

    type CharacterModel =
        { CharacterState : CharacterState
          CharacterAnimationState : CharacterAnimationState }

    type [<NoEquality; NoComparison>] ActionCommand =
        { Action : ActionType
          Source : Entity
          TargetOpt : Entity option }

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
        { CharacterModels : CharacterModel list
          BattleState : BattleState }

    and [<NoComparison>] BattleMessage =
        | Tick
        | ReticlesSelect of Entity * int

    and [<NoComparison>] IndexedCommand =
        | RegularMenuShow
        | RegularMenuSelect of string
        | SpecialMenuSelect of string
        | ItemMenuSelect of string
        | ReticlesCancel

    and [<NoComparison>] BattleCommand =
        | InitializeBattle
        | FinalizeBattle
        | ReadyCharacters
        | PoiseCharacters
        | CelebrateCharacters of bool
        | AdvanceCharacters
        | AttackCharacter of Entity * Entity
        | DamageCharacter of Entity
        | PoiseCharacter of Entity
        | WoundCharacter of Entity
        | DestroyCharacter of Entity
        | ResetCharacter of Entity
        | FadeSong
        | IndexedCommand of IndexedCommand * int

    and Screen with

        member this.GetBattleState = this.GetModel<BattleState>
        member this.SetBattleState = this.SetModel<BattleState>
        member this.BattleState = this.Model<BattleState>

    and BattleDispatcher () =
        inherit ScreenDispatcher<BattleModel, BattleMessage, BattleCommand> ({ CharacterModels = []; BattleState = BattleReady 0L })

        static member tickAttack (source : Entity) (targetOpt : Entity option) time timeLocal battleRunning world =

            // if target available, proceed with attack
            match targetOpt with
            | Some target when source.GetExists world && target.GetExists world ->
                match timeLocal with
                | 0L when (target.GetCharacterState world).IsHealthy ->
                    if target.CharacterState.GetBy (fun state -> state.IsHealthy) world then
                        withCmd battleRunning (AttackCharacter (source, target))
                    else // target wounded, cancel attack
                        let battleRunning = { battleRunning with CurrentCommandOpt = None }
                        withCmds battleRunning [ResetCharacter source; PoiseCharacter source]
                | _ when timeLocal = 1L * int64 (source.CharacterAnimationState.Get world).Stutter ->
                    withCmd battleRunning (DamageCharacter target)
                | _ ->
                    if CharacterAnimationState.finished time (source.GetCharacterAnimationState world) then
                        if (target.GetCharacterState world).IsHealthy then
                            let battleRunning = { battleRunning with CurrentCommandOpt = None }
                            withCmds battleRunning [PoiseCharacter source; PoiseCharacter target]
                        else
                            let woundCommand = CurrentCommand.make time (ActionCommand.make Wound target None)
                            let battleRunning = { battleRunning with CurrentCommandOpt = Some woundCommand }
                            withCmd battleRunning (PoiseCharacter source)
                    else just battleRunning

            // else target destroyed, cancel attack
            | Some _ | None ->
                let battleRunning = { battleRunning with CurrentCommandOpt = None }
                withCmds battleRunning [ResetCharacter source; PoiseCharacter source]

        static member tickWound (character : Entity) time timeLocal battleRunning world =
            if character.GetExists world then
                match timeLocal with
                | 0L ->
                    withCmd battleRunning (DamageCharacter character)
                | _ ->
                    let state = character.GetCharacterAnimationState world
                    if (character.GetCharacterState world).IsAlly then
                        match state.CharacterAnimationCycle with
                        | DamageCycle ->
                            if CharacterAnimationState.finished time state
                            then withCmd { battleRunning with CurrentCommandOpt = None } (WoundCharacter character)
                            else just battleRunning
                        | _ -> failwithumf ()
                    else
                        match state.CharacterAnimationCycle with
                        | DamageCycle ->
                            if CharacterAnimationState.finished time state
                            then withCmd battleRunning (WoundCharacter character)
                            else just battleRunning
                        | WoundCycle ->
                            if CharacterAnimationState.finished time state
                            then withCmd { battleRunning with CurrentCommandOpt = None } (DestroyCharacter character)
                            else just battleRunning
                        | _ -> failwithumf ()
            else just battleRunning

        static member tickReady state timeStart world =
            let time = World.getTickTime world
            let timeLocal = time - timeStart
            match timeLocal with
            | 0L -> withCmd (BattleRunning (BattleRunning.make ())) ReadyCharacters
            | 30L -> withCmd (BattleRunning (BattleRunning.make ())) PoiseCharacters
            | _ -> just state

        static member tickCurrentCommand currentCommand battleRunning world =
            let time = World.getTickTime world
            let timeLocal = time - currentCommand.TimeStart
            match currentCommand.ActionCommand.Action with
            | Attack ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                let (battleRunning, commands) = BattleDispatcher.tickAttack source targetOpt time timeLocal battleRunning world
                (BattleRunning battleRunning, commands)
            | Defend -> just (BattleRunning battleRunning)
            | Consume _ -> just (BattleRunning battleRunning)
            | Special _ -> just (BattleRunning battleRunning)
            | Wound ->
                let source = currentCommand.ActionCommand.Source
                let (battleRunning, signal) = BattleDispatcher.tickWound source time timeLocal battleRunning world
                match battleRunning.CurrentCommandOpt with
                | Some _ ->
                    // keep ticking wound
                    withSig (BattleRunning battleRunning) signal
                | None ->
                    let allies = World.getAllies Simulants.Scene world
                    let enemies = World.getEnemies Simulants.Scene world
                    let (state, signal2) =
                        if Seq.forall (fun (character : Entity) -> (character.GetCharacterState world).IsWounded) allies
                        then BattleDispatcher.tick (BattleCease (false, time)) world // tick for frame 0
                        elif Seq.forall (fun (character : Entity) -> (character.GetCharacterState world).IsWounded) enemies
                        then BattleDispatcher.tick (BattleCease (true, time)) world // tick for frame 0
                        else just (BattleRunning battleRunning)
                    withSig state (signal + signal2)

        static member tickNoCommand battleRunning world =
            match battleRunning.ActionQueue with
            | Queue.Cons (currentCommand, nextCommands) ->
                let time = World.getTickTime world
                let command = CurrentCommand.make time currentCommand
                let battleRunning = { battleRunning with CurrentCommandOpt = Some command; ActionQueue = nextCommands }
                BattleDispatcher.tick (BattleRunning battleRunning) world // tick for frame 0
            | Queue.Nil ->
                let allies = World.getAllies Simulants.Scene world
                let (battleRunning, allyCommands) =
                    Seq.fold (fun (battleRunning, commands) (ally : Entity) ->
                        let index = (ally.GetCharacterState world).PartyIndex
                        if ally.GetActionTimeNp world = Constants.Battle.ActionTime then (battleRunning, IndexedCommand (RegularMenuShow, index) :: commands)
                        else (battleRunning, commands))
                        (battleRunning, [])
                        allies
                let enemies = World.getEnemies Simulants.Scene world
                let (battleRunning, enemyCommands) =
                    Seq.fold (fun (battleRunning, commands) (enemy : Entity) ->
                        let allies = World.getAllies Simulants.Scene world
                        let allyIndex = (Random ()).Next allies.Length
                        let ally = allies.[allyIndex]
                        let attack = { Action = Attack; Source = enemy; TargetOpt = Some ally }
                        if enemy.GetActionTimeNp world = Constants.Battle.ActionTime then
                            let battleRunning = { battleRunning with ActionQueue = Queue.conj attack battleRunning.ActionQueue }
                            (battleRunning, ResetCharacter enemy :: commands)
                        else (battleRunning, commands))
                        (battleRunning, [])
                        enemies
                let commands = AdvanceCharacters :: enemyCommands @ allyCommands
                withCmds (BattleRunning battleRunning) commands

        static member tickRunning battleRunning world =
            match battleRunning.CurrentCommandOpt with
            | Some currentCommand -> BattleDispatcher.tickCurrentCommand currentCommand battleRunning world
            | None -> BattleDispatcher.tickNoCommand battleRunning world

        static member tickCease (state : BattleState) outcome timeStart world =
            let time = World.getTickTime world
            let timeLocal = time - timeStart
            match timeLocal with
            | 0L -> withCmd state (CelebrateCharacters outcome)
            | _ -> just state

        static member tick state world =
            match state with
            | BattleReady timeStart -> BattleDispatcher.tickReady state timeStart world
            | BattleRunning battleRunning -> BattleDispatcher.tickRunning battleRunning world
            | BattleCease (outcome, timeStart) -> BattleDispatcher.tickCease state outcome timeStart world

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
             reticles.TargetSelectEvent =|> fun evt -> ReticlesSelect (evt.Data, index)
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
            [battle.UpdateEvent => Tick
             battle.SelectEvent =>! InitializeBattle
             battle.DeselectEvent =>! FinalizeBattle
             battle.OutgoingStartEvent =>! FadeSong] @
             BattleDispatcher.inputBindings 0 @
             BattleDispatcher.inputBindings 1 @
             BattleDispatcher.inputBindings 2

        override this.Message (message, state, _, world) =
            match message with
            | Tick ->
                if World.isTicking world
                then BattleDispatcher.tick state world
                else just state
            | ReticlesSelect (enemy, index) ->
                let ally = Simulants.Ally index
                match state with
                | BattleRunning battleRunning ->
                    let command = ActionCommand.make Attack ally (Some enemy)
                    let state = BattleRunning { battleRunning with ActionQueue = Queue.conj command battleRunning.ActionQueue }
                    withCmd state (ResetCharacter ally)
                | _ -> just state

        override this.Command (command, _, battle, world) =
            match command with
            | InitializeBattle ->
                let world = World.hintRenderPackageUse Assets.BattlePackage world
                let world = World.hintAudioPackageUse Assets.BattlePackage world
                let world = World.playSong 0 (1.0f * Constants.Audio.MasterSongVolume) Assets.BattleSong world
                just (battle.SetBattleState (BattleReady (World.getTickTime world)) world)
            | FinalizeBattle ->
                let world = World.hintRenderPackageDisuse Assets.BattlePackage world
                just (World.hintAudioPackageDisuse Assets.BattlePackage world)
            | ReadyCharacters ->
                let time = World.getTickTime world
                let characters = World.getCharacters Simulants.Scene world
                let world =
                    Seq.fold (fun world (character : Entity) ->
                        character.CharacterAnimationState.Update (CharacterAnimationState.setCycle (Some time) ReadyCycle) world)
                        world characters
                just world
            | PoiseCharacters ->
                let time = World.getTickTime world
                let characters = World.getCharacters Simulants.Scene world
                let world =
                    Seq.fold (fun world (character : Entity) ->
                        character.CharacterAnimationState.Update (CharacterAnimationState.setCycle (Some time) PoiseCycle) world)
                        world characters
                just world
            | CelebrateCharacters outcome ->
                let time = World.getTickTime world
                let allies =
                    if outcome
                    then World.getAllies Simulants.Scene world
                    else World.getEnemies Simulants.Scene world
                let world =
                    Seq.fold (fun world (character : Entity) ->
                        character.CharacterAnimationState.Update (CharacterAnimationState.setCycle (Some time) CelebrateCycle) world)
                        world allies
                just world
            | AdvanceCharacters ->
                let characters = World.getCharacters Simulants.Scene world
                let world =
                    List.fold (fun world (character : Entity) ->
                        character.ActionTimeNp.Update ((+) Constants.Battle.ActionTimeInc) world)
                        world characters
                just world
            | AttackCharacter (source, target) ->
                let time = World.getTickTime world
                let world = source.CharacterAnimationState.Update (CharacterAnimationState.setCycle (Some time) AttackCycle) world
                let world =
                    target.CharacterState.Update (fun state ->
                        let rom = Simulants.Game.GetModel world
                        let power = source.CharacterState.GetBy (fun state -> state.ComputePower rom) world
                        let shield = state.ComputeShield rom
                        let damage = max 0 (int (Math.Ceiling (double (power - shield))))
                        let hitPoints = (target.GetCharacterState world).HitPoints
                        let hitPoints =  max 0 (hitPoints - damage)
                        { state with HitPoints = hitPoints })
                        world
                if target.CharacterState.GetBy (fun state -> state.HitPoints = 0 && state.IsAlly) world
                then withCmd world (ResetCharacter target)
                else just world
            | DamageCharacter character ->
                let time = World.getTickTime world
                just (character.CharacterAnimationState.Update (CharacterAnimationState.setCycle (Some time) DamageCycle) world)
            | PoiseCharacter character ->
                let time = World.getTickTime world
                just (character.CharacterAnimationState.Update (CharacterAnimationState.setCycle (Some time) PoiseCycle) world)
            | WoundCharacter character ->
                let time = World.getTickTime world
                just (character.CharacterAnimationState.Update (CharacterAnimationState.setCycle (Some time) WoundCycle) world)
            | DestroyCharacter character ->
                just (World.destroyEntity character world)
            | ResetCharacter character ->
                let world =
                    if (character.GetCharacterState world).IsAlly then
                        let index = (character.GetCharacterState world).PartyIndex
                        let entities = Simulants.AllInputEntities index
                        List.fold (fun world (entity : Entity) -> entity.SetVisible false world) world entities
                    else world
                just (character.SetActionTimeNp 0 world)
            | FadeSong ->
                just (World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world)
            | IndexedCommand (command, index) ->
                this.IndexedCommand (command, index, battle, world)

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
            [Content.layerFromFile Simulants.Scene.Name Assets.BattleSceneLayerFilePath
             BattleDispatcher.inputContent 0
             BattleDispatcher.inputContent 1
             BattleDispatcher.inputContent 2
             Content.layer "" []
                [Content.entities (model.MapOut $ fun model -> seq model.CharacterModels) $ fun characterModel ->
                    Content.entity<CharacterDispatcher> (Address.getName characterModel.This.ParticipantAddress)
                        [Entity.CharacterState ==> characterModel.MapOut (fun model -> model.CharacterState)
                         Entity.CharacterAnimationState ==> characterModel.MapOut (fun model -> model.CharacterAnimationState)]]]