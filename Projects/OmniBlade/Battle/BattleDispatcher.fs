// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module BattleDispatcher =

    type Screen with
        member this.GetBattle world = this.GetModelGeneric<Battle> world
        member this.SetBattle value world = this.SetModelGeneric<Battle> value world
        member this.Battle = this.ModelGeneric<Battle> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<Battle, BattleMessage, BattleCommand> (Battle.empty : Battle)

        static let displayEffect (delay : int64) size positioning layering descriptor screen world =
            World.schedule delay (fun world ->
                let (entity, world) = World.createEntity<Effect2dDispatcher> DefaultOverlay None Simulants.BattleScene world
                let world = entity.SetSize size world
                let world =
                    match positioning with
                    | Position position -> entity.SetPosition position world
                    | Center center -> entity.SetPerimeterCenter center world
                    | Bottom bottom -> entity.SetPerimeterBottom bottom world
                let world =
                    match layering with
                    | Under -> entity.SetElevation Constants.Battle.EffectElevationUnder world
                    | Over -> entity.SetElevation Constants.Battle.EffectElevationOver world
                let world = entity.SetSelfDestruct true world
                let world = entity.SetEffectDescriptor descriptor world
                world)
                screen world

        override this.Initialize (_, _) =
            [Screen.UpdateEvent => Update
             Screen.PostUpdateEvent => UpdateEye
             Simulants.BattleRide.EffectTagTokens.ChangeEvent =|> fun evt -> UpdateRideTokens (evt.Data.Value :?> Map<string, Effects.Slice>)]

        override this.Message (battle, message, _, _) =

            match message with
            | Update ->
                Battle.advance battle

            | UpdateRideTokens rideTokens ->
                match Map.tryFind "Tag" rideTokens with
                | Some rideToken ->
                    match battle.CurrentCommandOpt with
                    | Some command ->
                        let character = command.ActionCommand.SourceIndex
                        let battle = Battle.updateCharacterBottom (constant rideToken.Position) character battle
                        just battle
                    | None -> just battle
                | None -> just battle

            | InteractDialog ->
                match battle.DialogOpt with
                | Some dialog ->
                    match Dialog.tryAdvance id dialog with
                    | (true, dialog) ->
                        let battle = Battle.updateDialogOpt (constant (Some dialog)) battle
                        just battle
                    | (false, _) ->
                        let battle = Battle.updateDialogOpt (constant None) battle
                        just battle
                | None -> just battle

            | RegularItemSelect (characterIndex, item) ->
                let battle =
                    match item with
                    | "Attack" ->
                        battle |>
                        Battle.updateCharacterInputState (constant (AimReticles (item, EnemyAim true))) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | "Defend" ->
                        let battle = Battle.updateCharacterInputState (constant NoInput) characterIndex battle
                        let command = ActionCommand.make Defend characterIndex None None
                        let battle = Battle.appendActionCommand command battle
                        battle
                    | "Tech" ->
                        battle |>
                        Battle.updateCharacterInputState (constant TechMenu) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | "Consumable" ->
                        battle |>
                        Battle.updateCharacterInputState (constant ItemMenu) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | _ -> failwithumf ()
                just battle

            | RegularItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle

            | ConsumableItemSelect (characterIndex, item) ->
                let consumableType =
                    scvalue<ConsumableType> item
                let aimType =
                    match Data.Value.Consumables.TryGetValue consumableType with
                    | (true, consumableData) -> consumableData.AimType
                    | (false, _) -> NoAim
                let battle = Battle.updateCharacterInputState (constant (AimReticles (item, aimType))) characterIndex battle
                just battle

            | ConsumableItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle

            | TechItemSelect (characterIndex, item) ->
                let techType =
                    scvalue<TechType> item
                let aimType =
                    match Data.Value.Techs.TryGetValue techType with
                    | (true, techData) -> techData.AimType
                    | (false, _) -> NoAim
                let battle = Battle.updateCharacterInputState (constant (AimReticles (item, aimType))) characterIndex battle
                just battle

            | TechItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle

            | ReticlesSelect (sourceIndex, targetIndex) ->
                match battle.BattleState with
                | BattleRunning ->
                    let battle = Battle.confirmCharacterInput sourceIndex targetIndex battle
                    let battle = Battle.resetCharacterActionTime sourceIndex battle
                    let battle = Battle.resetCharacterInput sourceIndex battle
                    just battle
                | _ -> just battle

            | ReticlesCancel characterIndex ->
                let battle = Battle.cancelCharacterInput characterIndex battle
                just battle

            | Nop -> just battle

        override this.Command (battle, command, screen, world) =

            match command with
            | UpdateEye ->
                let world = World.setEye2dCenter v2Zero world
                just world

            | BattleCommand.PlaySound (delay, volume, sound) ->
                let world = World.schedule delay (World.playSound volume sound) screen world
                just world

            | BattleCommand.PlaySong (fadeIn, fadeOut, start, volume, assetTag) ->
                let world = World.playSong fadeIn fadeOut start volume assetTag world
                just world

            | BattleCommand.FadeOutSong fade ->
                let world = World.fadeOutSong fade world
                just world

            | DisplayHop (hopStart, hopStop) ->
                let descriptor = EffectDescriptors.hop hopStart hopStop
                let (entity, world) = World.createEntity<Effect2dDispatcher> DefaultOverlay (Some Simulants.BattleRide.Surnames) Simulants.BattleScene world
                let world = entity.SetSelfDestruct true world
                let world = entity.SetEffectDescriptor descriptor world
                just world

            | DisplayCircle (position, radius) ->
                let descriptor = EffectDescriptors.circle radius
                let (entity, world) = World.createEntity<Effect2dDispatcher> DefaultOverlay (Some Simulants.BattleRide.Surnames) Simulants.BattleScene world
                let world = entity.SetPosition position world
                let world = entity.SetSelfDestruct true world
                let world = entity.SetEffectDescriptor descriptor world
                just world

            | DisplayHitPointsChange (targetIndex, delta) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let (entity, world) = World.createEntity<Effect2dDispatcher> DefaultOverlay None Simulants.BattleScene world
                    let world = entity.SetPosition target.BottomOriginalOffset4 world
                    let world = entity.SetElevation Constants.Battle.GuiEffectElevation world
                    let world = entity.SetSelfDestruct true world
                    let world = entity.SetEffectDescriptor (EffectDescriptors.hitPointsChange delta) world
                    just world
                | None -> just world

            | DisplayCancel targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let (entity, world) = World.createEntity<Effect2dDispatcher> DefaultOverlay None Simulants.BattleScene world
                    let world = entity.SetPosition target.CenterOffset4 world
                    let world = entity.SetElevation (Constants.Battle.GuiEffectElevation + 1.0f) world
                    let world = entity.SetSelfDestruct true world
                    let world = entity.SetEffectDescriptor EffectDescriptors.cancel world
                    just world
                | None -> just world

            | DisplayCut (delay, light, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 144.0f 0.0f) (Bottom target.Bottom) Over (EffectDescriptors.cut light) screen world |> just
                | None -> just world

            | DisplayCritical (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 144.0f 0.0f) (Bottom (target.Bottom + v3Up * 3.0f)) Over EffectDescriptors.critical screen world |> just
                | None -> just world

            | DisplayHeavyCritical (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 144.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.heavyCritical screen world |> just
                | None -> just world

            | DisplayPoisonCut (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 144.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.poisonCut screen world |> just
                | None -> just world

            | DisplayPowerCut (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 144.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.powerCut screen world |> just
                | None -> just world

            | DisplayDispelCut (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 96.0f 144.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.dispelCut screen world |> just
                | None -> just world

            | DisplayDoubleCut (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 96.0f 144.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.doubleCut screen world |> just
                | None -> just world

            | DisplaySlashSpike (delay, bottom, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let projection = Vector3.Normalize (target.Bottom - bottom) * single Constants.Render.VirtualResolutionX + target.Bottom
                    let world = displayEffect delay (v3 96.0f 96.0f 0.0f) (Bottom bottom) Over (EffectDescriptors.slashSpike bottom projection) screen world
                    just world
                | None -> just world

            | DisplaySlashTwister (delay, bottom, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let projection = Vector3.Normalize (target.Bottom - bottom) * single Constants.Render.VirtualResolutionX + target.Bottom
                    let world = displayEffect delay (v3 96.0f 96.0f 0.0f) (Bottom bottom) Over (EffectDescriptors.slashWind bottom projection) screen world
                    just world
                | None -> just world

            | DisplayCycloneBlur (delay, targetIndex, radius) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 234.0f 234.0f 0.0f) (Center target.Center) Over (EffectDescriptors.cycloneBlur radius) screen world |> just
                | None -> just world

            | DisplayBuff (delay, statusType, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) Over (EffectDescriptors.buff statusType) screen world |> just
                | None -> just world

            | DisplayDebuff (delay, statusType, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) Over (EffectDescriptors.debuff statusType) screen world |> just
                | None -> just world

            | DisplayImpactSplash (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 96.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.impactSplash screen world |> just
                | None -> just world

            | DisplayArcaneCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 300.0f 300.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 120.0f 0.0f)) Over EffectDescriptors.arcaneCast screen world |> just
                | None -> just world

            | DisplayHolyCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 300.0f 300.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 100.0f 0.0f)) Over EffectDescriptors.holyCast screen world |> just
                | None -> just world

            | DisplayDimensionalCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom source.Bottom) Over EffectDescriptors.dimensionalCast screen world |> just
                | None -> just world

            | DisplayGenericCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 98.0f 98.0f 0.0f) (Bottom source.Bottom) Over EffectDescriptors.genericCast screen world |> just
                | None -> just world

            | DisplayFire (delay, sourceIndex, targetIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        let descriptor = EffectDescriptors.fire (source.Bottom + v3 80.0f 80.0f 0.0f) (target.Bottom + v3 0.0f 20.0f 0.0f)
                        let world = displayEffect delay (v3 100.0f 100.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 50.0f 0.0f)) Over descriptor screen world
                        just world
                    | None -> just world
                | None -> just world

            | DisplayFlame (delay, sourceIndex, targetIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        let descriptor = EffectDescriptors.flame source.CenterOffset target.CenterOffset
                        let world = displayEffect delay (v3 144.0f 144.0f 0.0f) (Bottom source.Bottom) Over descriptor screen world
                        just world
                    | None -> just world
                | None -> just world

            | DisplayIce (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.ice screen world |> just
                | None -> just world

            | DisplaySnowball (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 432.0f 432.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.snowball screen world |> just
                | None -> just world

            | DisplayBolt (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 758.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.bolt screen world |> just
                | None -> just world

            | DisplayCure (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.cure screen world |> just
                | None -> just world
            
            | DisplayProtect (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.protect screen world |> just
                | None -> just world
            
            | DisplayPurify (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 192.0f 0.0f) (Bottom (target.Bottom - v3 0.0f 100.0f 0.0f)) Over EffectDescriptors.purify screen world |> just
                | None -> just world

            | DisplayInferno delay ->
                displayEffect delay (v3 48.0f 48.0f 0.0f) (Position (v3 0.0f 0.0f 0.0f)) Over EffectDescriptors.inferno screen world |> just

            | DisplayScatterBolt delay ->
                let origin = v2 -288.0f -240.0f // TODO: P1: turn these duplicated vars into global consts.
                let tile = v2 48.0f 48.0f
                let (w, h) = (14, 8)
                let position = v3 (origin.X + single (Gen.random1 w) * tile.X) (origin.Y + single (Gen.random1 h) * tile.Y) 0.0f
                displayEffect delay (v3 192.0f 758.0f 0.0f) (Bottom position) Over EffectDescriptors.bolt screen world |> just

            | DisplaySilk (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 144.0f 144.0f 0.0f) (Bottom target.Bottom) Over EffectDescriptors.silk screen world |> just
                | None -> just world

        override this.Content (battle, _) =

            [// scene group
             Content.group Simulants.BattleScene.Name []

                [// tile map
                 Content.tileMap "TileMap"
                    [Entity.Position == v3 -480.0f -270.0f 0.0f
                     Entity.Elevation == Constants.Battle.BackgroundElevation
                     Entity.TileMap := battle.TileMap
                     Entity.TileIndexOffset := battle.TileIndexOffset
                     Entity.TileIndexOffsetRange := battle.TileIndexOffsetRange]

                 // message
                 yield! Content.dialog "Message"
                    (Constants.Battle.GuiOutputElevation) Nop Nop id
                    (match battle.MessageOpt with Some (_, _, dialog) -> Some dialog | None -> None)

                 // dialog
                 yield! Content.dialog "Dialog"
                    (Constants.Battle.GuiOutputElevation) Nop Nop id
                    (match battle.DialogOpt with Some dialog -> Some dialog | None -> None)

                 // dialog interact button
                 Content.button "DialogInteract"
                    [Entity.Position == v3 248.0f -240.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage
                     Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible := match battle.DialogOpt with Some dialog -> Dialog.canAdvance id dialog | None -> false
                     Entity.Text == "Next"
                     Entity.ClickEvent => InteractDialog]

                 // characters
                 for (index, character) in (Battle.getCharacters battle).Pairs do

                    // character
                    let characterPlus = CharacterPlus.make battle.UpdateTime character
                    Content.entity<CharacterDispatcher> (CharacterIndex.toEntityName index) [Entity.CharacterPlus := characterPlus]

                 // hud
                 for (index, character) in (Battle.getCharactersHudded battle).Pairs do

                    // bars
                    Content.composite (CharacterIndex.toEntityName index + "+Hud") []

                        [// health bar
                         for i in 0 .. dec 2 do
                            Content.fillBar ("HealthBar+" + string i)
                                [Entity.MountOpt == None
                                 Entity.Size == v3 48.0f 6.0f 0.0f
                                 Entity.PerimeterCenter := character.BottomOriginalOffset
                                 Entity.Elevation := if i = 0 then Constants.Battle.GuiBackgroundElevation else Constants.Battle.GuiForegroundElevation
                                 Entity.Fill := single character.HitPoints / single character.HitPointsMax
                                 Entity.FillInset := 1.0f / 12.0f
                                 Entity.FillColor :=
                                    (let pulseTime = battle.UpdateTime % Constants.Battle.CharacterFillColorPulseDuration
                                     let pulseProgress = single pulseTime / single Constants.Battle.CharacterFillColorPulseDuration
                                     let pulseIntensity = byte (sin (pulseProgress * MathF.PI) * 127.0f) / byte 2 + byte 32
                                     match character.AutoBattleOpt with
                                     | Some autoBattle ->
                                        if autoBattle.AutoTechOpt.IsSome then Color.Red.WithA8 pulseIntensity
                                        elif autoBattle.ChargeTech then Color.Purple.WithA8 pulseIntensity
                                        elif character.Statuses.ContainsKey Poison then Color.LawnGreen.WithA8 pulseIntensity
                                        else Color.Red.WithA8 (byte 95)
                                     | None ->
                                        if character.Statuses.ContainsKey Poison
                                        then Color.LawnGreen.WithA8 pulseIntensity
                                        else Color.Red.WithA8 (byte 95))
                                 Entity.BorderImage == Assets.Gui.HealthBorderImage
                                 Entity.BorderColor := color8 (byte 60) (byte 60) (byte 60) (byte 191)]

                         // tech bar
                         for i in 0 .. dec 2 do
                            if character.Ally then
                                Content.fillBar ("TechBar+" + string i)
                                    [Entity.MountOpt == None
                                     Entity.Size == v3 48.0f 6.0f 0.0f
                                     Entity.PerimeterCenter := character.BottomOriginalOffset2
                                     Entity.Elevation := if i = 0 then Constants.Battle.GuiBackgroundElevation else Constants.Battle.GuiForegroundElevation
                                     Entity.Fill := single character.TechPoints / single character.TechPointsMax
                                     Entity.FillInset := 1.0f / 12.0f
                                     Entity.FillColor == (color8 (byte 74) (byte 91) (byte 169) (byte 127)).WithA8 (byte 95)
                                     Entity.BorderImage == Assets.Gui.TechBorderImage
                                     Entity.BorderColor == color8 (byte 60) (byte 60) (byte 60) (byte 191)]]]

             // inputs condition
             if battle.Running then

                // inputs group
                Content.group Simulants.BattleInputs.Name []

                    [// inputs
                     for (index, ally) in (Battle.getCharactersHealthy battle).Pairs do

                        // input
                        Content.composite (CharacterIndex.toEntityName index + "+Input") []
                            [match ally.CharacterInputState with
                             | RegularMenu ->
                                Content.entity<RingMenuDispatcher> "RegularMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiInputElevation
                                     Entity.Enabled :=
                                        (let allies = battle |> Battle.getAllies |> Map.toValueList
                                         let alliesPastRegularMenu =
                                            Seq.notExists (fun (ally : Character) ->
                                                match ally.CharacterInputState with NoInput | RegularMenu -> false | _ -> true)
                                                allies
                                         alliesPastRegularMenu)
                                     Entity.RingMenu == { Items = Map.ofList [("Attack", (0, true)); ("Tech", (1, true)); ("Consumable", (2, true)); ("Defend", (3, true))]; Cancellable = false }
                                     Entity.ItemSelectEvent =|> fun evt -> RegularItemSelect (index, evt.Data) |> signal
                                     Entity.CancelEvent => RegularItemCancel index]
                             | ItemMenu ->
                                Content.entity<RingMenuDispatcher> "ConsumableMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiInputElevation
                                     Entity.RingMenu :=
                                        (let consumables =
                                            battle.Inventory |>
                                            Inventory.getConsumables |>
                                            Map.ofSeqBy (fun kvp -> (getCaseName kvp.Key, (getCaseTag kvp.Key, true)))
                                         { Items = consumables; Cancellable = true })
                                     Entity.ItemSelectEvent =|> fun evt -> ConsumableItemSelect (index, evt.Data) |> signal
                                     Entity.CancelEvent => ConsumableItemCancel index]
                             | TechMenu ->
                                Content.entity<RingMenuDispatcher> "TechMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiInputElevation
                                     Entity.RingMenu :=
                                        (let techs =
                                            ally.Techs |>
                                            Map.ofSeqBy (fun tech ->
                                                let affordable =
                                                    match Map.tryFind tech Data.Value.Techs with
                                                    | Some techData -> techData.TechCost <= ally.TechPoints && not (Map.containsKey Silence ally.Statuses)
                                                    | None -> false
                                                let castable =
                                                    if tech.ConjureTech then
                                                        match ally.ConjureChargeOpt with
                                                        | Some conjureCharge -> conjureCharge >= Constants.Battle.ChargeMax
                                                        | None -> true
                                                    else true
                                                let usable = affordable && castable
                                                (getCaseName tech, (getCaseTag tech, usable)))
                                         { Items = techs; Cancellable = true })
                                     Entity.ItemSelectEvent =|> fun evt -> TechItemSelect (index, evt.Data) |> signal
                                     Entity.CancelEvent => TechItemCancel index]
                             | AimReticles _ ->
                                Content.entity<ReticlesDispatcher> "Reticles"
                                    [Entity.Elevation == Constants.Battle.GuiInputElevation
                                     Entity.Reticles :=
                                        (let aimType =
                                            match Battle.tryGetCharacter index battle with
                                            | Some character -> character.CharacterInputState.AimType
                                            | None -> NoAim
                                         let characters = Battle.getTargets aimType battle
                                         let reticles =
                                            Map.map (fun _ (c : Character) ->
                                                match c.Stature with
                                                | BossStature -> c.CenterOffset2
                                                | _ -> c.CenterOffset)
                                                characters
                                         reticles)
                                     Entity.TargetSelectEvent =|> fun evt -> ReticlesSelect (index, evt.Data) |> signal
                                     Entity.CancelEvent => ReticlesCancel index]
                             | NoInput -> ()]]]