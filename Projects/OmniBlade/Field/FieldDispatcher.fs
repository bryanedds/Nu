// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module FieldDispatcher =

    type FieldMessage =
        | Update
        | UpdateFieldTransition
        | UpdateAvatarBodyTracking
        | AvatarBodyTransform of BodyTransformData
        | AvatarBodyCollision of BodyCollisionData
        | AvatarBodySeparationImplicit of BodySeparationImplicitData
        | AvatarBodySeparationExplicit of BodySeparationExplicitData
        | ScreenTransitioning of bool
        | MenuTeamOpen
        | MenuTeamAlly of int
        | MenuInventoryOpen
        | MenuInventoryPageUp
        | MenuInventoryPageDown
        | MenuInventorySelect of int * (ItemType * int Option)
        | MenuInventoryUse of int
        | MenuInventoryCancel
        | MenuTechsOpen
        | MenuTechsAlly of int
        | MenuTechsSelect of int
        | MenuKeyItemsOpen
        | MenuKeyItemsPageUp
        | MenuKeyItemsPageDown
        | MenuKeyItemsSelect of int * (ItemType * int Option)
        | MenuOptionsOpen
        | MenuOptionsSelectBattleSpeed of BattleSpeed
        | MenuClose
        | ShopBuy
        | ShopSell
        | ShopPageUp
        | ShopPageDown
        | ShopSelect of int * (ItemType * int Option)
        | ShopConfirmAccept
        | ShopConfirmDecline
        | ShopLeave
        | PromptLeft
        | PromptRight
        | TryBattle of BattleType * Advent Set
        | Interact
        interface Message

    type FieldCommand =
        | ProcessKeyInput
        | ProcessTouchInput of Vector2
        | UpdateEye
        | WarpAvatar of Vector3
        | MoveAvatar of Vector3
        | FaceAvatar of Direction
        | PlayFieldSong
        | PlaySound of int64 * single * Sound AssetTag
        | PlaySong of int64 * int64 * int64 * single * Song AssetTag
        | FadeOutSong of int64
        | Nop
        interface Command

    type Screen with
        member this.GetField world = this.GetModelGeneric<Field> world
        member this.SetField value world = this.SetModelGeneric<Field> value world
        member this.Field = this.ModelGeneric<Field> ()

    type FieldDispatcher () =
        inherit ScreenDispatcher<Field, FieldMessage, FieldCommand> (fun world -> Field.empty (World.getUpdateTime world) (World.getViewBounds2dAbsolute world))

        static let isIntersectedProp (collider : ShapeIndex) (collidee : ShapeIndex) world =
            let collideeEntity = collidee.BodyId.BodySource :?> Entity
            if (collider.ShapeIndex = Constants.Field.AvatarCollisionShapeIndex &&
                collideeEntity.Exists world &&
                collideeEntity.Is<PropDispatcher> world &&
                match (collideeEntity.GetPropPlus world).Prop.PropData with
                | Portal _ -> true
                | Sensor _ -> true
                | _ -> false) then
                true
            elif (collider.ShapeIndex = Constants.Field.AvatarSensorShapeIndex &&
                  collideeEntity.Exists world &&
                  collideeEntity.Is<PropDispatcher> world &&
                  match (collideeEntity.GetPropPlus world).Prop.PropData with
                  | Portal _ -> false
                  | Sensor _ -> false
                  | _ -> true) then
                true
            else false

        override this.Initialize (_, _) =
            [Screen.UpdateEvent => ProcessKeyInput
             Screen.UpdateEvent => Update
             Screen.PostUpdateEvent => UpdateFieldTransition
             Screen.PostUpdateEvent => UpdateEye
             Screen.PostUpdateEvent => UpdateAvatarBodyTracking
             Screen.SelectEvent => PlayFieldSong
             Screen.IncomingStartEvent => ScreenTransitioning true
             Screen.IncomingFinishEvent => ScreenTransitioning false
             Screen.OutgoingStartEvent => ScreenTransitioning true
             Screen.OutgoingFinishEvent => ScreenTransitioning false
             Simulants.FieldSceneAvatar.BodyTransformEvent =|> fun evt -> AvatarBodyTransform evt.Data |> signal
             Simulants.FieldSceneAvatar.BodyCollisionEvent =|> fun evt -> AvatarBodyCollision evt.Data |> signal
             Simulants.FieldSceneAvatar.BodySeparationImplicitEvent =|> fun evt -> AvatarBodySeparationImplicit evt.Data |> signal
             Simulants.FieldSceneAvatar.BodySeparationExplicitEvent =|> fun evt -> AvatarBodySeparationExplicit evt.Data |> signal]

        override this.Message (field, message, _, world) =

            match message with
            | Update ->

                // advance field time
                let field = Field.advanceUpdateTime field

                // advance cue and convert its signals
                let (cue, definitions, (cueSignals, field)) = Field.advanceCue field.Cue field.Definitions field
                let fieldSignals = List.map cast cueSignals
                let signals =
                    List.map (fun fieldSignal ->
                        match fieldSignal with
                        | FieldSignal.TryBattle (battleType, consequents) -> TryBattle (battleType, consequents) |> signal
                        | FieldSignal.PlaySound (delay, volume, sound) -> PlaySound (delay, volume, sound) |> signal
                        | FieldSignal.PlaySong (fadeInTime, fadeOutTime, songTime, volume, song) -> PlaySong (fadeInTime, fadeOutTime, songTime, volume, song) |> signal
                        | FieldSignal.FadeOutSong fadeOutTime -> FadeOutSong fadeOutTime |> signal)
                        fieldSignals

                // reset cue definitions if finished
                let field =
                    match cue with
                    | CueSystem.Fin -> Field.updateDefinitions (constant field.DefinitionsOriginal) field
                    | _ -> Field.updateDefinitions (constant definitions) field
                let field = Field.updateCue (constant cue) field

                // advance dialog
                let field =
                    match field.DialogOpt with
                    | Some dialog ->
                        let dialog = Dialog.advance (Field.detokenize field) (World.getUpdateTime world) dialog
                        Field.updateDialogOpt (constant (Some dialog)) field
                    | None -> field

                // advance portal
                let (signals, field) =
                    match field.FieldTransitionOpt with
                    | None ->
                        match Field.tryGetTouchingPortal field with
                        | Some (fieldType, destination, direction, isWarp) ->
                            if Option.isNone field.BattleOpt then // make sure we don't teleport if a battle is started earlier in the frame
                                let transition =
                                    { FieldType = fieldType
                                      FieldDestination = destination
                                      FieldDirection = direction
                                      FieldTransitionTime = field.UpdateTime + Constants.Field.TransitionTime }
                                let field = Field.updateFieldTransitionOpt (constant (Some transition)) field
                                let playSound =
                                    if isWarp
                                    then PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.StepWarpSound)
                                    else PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.StepStairSound)
                                (signal playSound :: signals, field)
                            else (signals, field)
                        | None -> (signals, field)
                    | Some _ -> (signals, field)

                // advance sensor
                let (signals, field) =
                    match field.FieldTransitionOpt with
                    | None ->
                        let sensors = Field.getTouchedSensors field
                        let results =
                            List.fold (fun (signals : Signal list, field : Field) (sensorType, cue, requirements) ->
                                if field.Advents.IsSupersetOf requirements then
                                    let field = Field.updateCue (constant cue) field
                                    match sensorType with
                                    | AirSensor -> (signals, field)
                                    | HiddenSensor | StepPlateSensor -> (signal (PlaySound (0L,  Constants.Audio.SoundVolumeDefault, Assets.Field.StepPlateSound)) :: signals, field)
                                else (signals, field))
                                (signals, field) sensors
                        results
                    | Some _ -> (signals, field)

                // advance spirits
                let (signals : Signal list, field) =
                    if  world.Advancing &&
                        field.Menu.MenuState = MenuClosed &&
                        CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.BattleOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt then
                        match Field.advanceSpirits field world with
                        | Left (battleData, field) ->
                            let time = field.UpdateTime
                            let playTime = Option.defaultValue time field.FieldSongTimeOpt
                            let startTime = time - playTime
                            let prizePool = { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }
                            let field = Field.enterBattle (World.getUpdateTime world) startTime prizePool battleData field
                            let fade = FadeOutSong 60L
                            let beastGrowl = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastGrowlSound)
                            (signal fade :: signal beastGrowl :: signals, field)
                        | Right field -> (signals, field)
                    else (signals, field)

                // fin
                (signals, field)

            | UpdateFieldTransition ->

                // check if transitioning
                match field.FieldTransitionOpt with
                | Some fieldTransition ->

                    // attempt to get field data for destination
                    match Data.Value.Fields.TryGetValue fieldTransition.FieldType with
                    | (true, destinationData) ->

                        // handle field transition
                        let time = field.UpdateTime
                        let currentSongOpt = world |> World.getCurrentSongOpt |> Option.map (fun song -> song.Song)
                        let (signals, field) =

                            // start transition
                            if time = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime then
                                match (currentSongOpt, destinationData.FieldSongOpt) with
                                | (Some song, Some song2) when assetEq song song2 -> just field
                                | (_, _) -> withSignal (FadeOutSong 30L) field

                            // half-way transition (fully blacked out)
                            elif time = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime / 2L + 1L then
                                match destinationData.FieldType with // HACK: pre-generate fields.
                                | CastleConnector -> for i in 0 .. 2 do FieldData.tryGetTileMap field.OmniSeedState (Data.Value.Fields.[Castle i]) |> ignore
                                | _ -> ()
                                let field = Field.updateFieldType (World.getUpdateTime world) (constant fieldTransition.FieldType) field
                                let field = Field.updateAvatar (Avatar.updateDirection (constant fieldTransition.FieldDirection)) field
                                let warpAvatar = WarpAvatar fieldTransition.FieldDestination
                                let songCmd =
                                    match Field.getFieldSongOpt field with
                                    | Some fieldSong ->
                                        match currentSongOpt with
                                        | Some song when assetEq song fieldSong -> Nop
                                        | _ -> PlaySong (0L, 30L, 0L, Constants.Audio.SongVolumeDefault, fieldSong)
                                    | None -> Nop
                                withSignals [warpAvatar; songCmd] field

                            // finish transition
                            elif time = fieldTransition.FieldTransitionTime then
                                let startTime = field.UpdateTime
                                let field = Field.updateFieldSongTimeOpt (constant (Some startTime)) field
                                let field = Field.updateFieldTransitionOpt (constant None) field
                                just field

                            // intermediate state
                            else just field

                        // update field reference to make sure transition binding actuates
                        let field = Field.updateReference field
                        (signals, field)

                    // no transition
                    | (false, _) -> just field

                // no transition
                | None -> just field
            
            | UpdateAvatarBodyTracking ->

                // clear all temporary avatar body shapes
                let field = Field.updateAvatarCollidedPropIds (constant []) field
                let field = Field.updateAvatarSeparatedPropIds (constant []) field
                just field

            | AvatarBodyTransform transform ->

                // update avatar from transform
                let time = World.getUpdateTime world
                let avatar = field.Avatar
                let avatar = Avatar.updateCenter (constant transform.BodyCenter) avatar
                let avatar =
                    let direction = Direction.ofVector3Biased transform.BodyLinearVelocity
                    let speed = transform.BodyLinearVelocity.Magnitude
                    if speed > Constants.Field.AvatarIdleSpeedMax then
                        if direction <> avatar.Direction || avatar.CharacterAnimationType = IdleAnimation then
                            let avatar = Avatar.updateDirection (constant direction) avatar
                            Avatar.animate time WalkAnimation avatar
                        else avatar
                    else Avatar.animate time IdleAnimation avatar
                just (Field.updateAvatar (constant avatar) field)

            | AvatarBodyCollision collision ->

                // add collided body shape
                let field =
                    if isIntersectedProp collision.BodyShapeCollider collision.BodyShapeCollidee world then
                        let field = Field.updateAvatarCollidedPropIds (List.cons ((collision.BodyShapeCollidee.BodyId.BodySource :?> Entity).GetPropPlus world).Prop.PropId) field
                        let field = Field.updateAvatarIntersectedPropIds (List.cons ((collision.BodyShapeCollidee.BodyId.BodySource :?> Entity).GetPropPlus world).Prop.PropId) field
                        field
                    else field
                just field

            | AvatarBodySeparationImplicit separation ->

                // add separated body shape
                match separation.BodyId.BodySource with
                | :? Entity as entity when entity.Is<PropDispatcher> world ->
                    let propId = (entity.GetPropPlus world).Prop.PropId
                    let (separatedPropIds, intersectedPropIds) = List.split ((=) propId) field.AvatarIntersectedPropIds
                    let field = Field.updateAvatarIntersectedPropIds (constant intersectedPropIds) field
                    let field = Field.updateAvatarSeparatedPropIds ((@) separatedPropIds) field
                    just field
                | _ -> just field

            | AvatarBodySeparationExplicit separation ->

                // add separated body shape
                let field =
                    if isIntersectedProp separation.BodyShapeSeparator separation.BodyShapeSeparatee world then
                        let field = Field.updateAvatarSeparatedPropIds (List.cons ((separation.BodyShapeSeparatee.BodyId.BodySource :?> Entity).GetPropPlus world).Prop.PropId) field
                        let field = Field.updateAvatarIntersectedPropIds (List.remove ((=) ((separation.BodyShapeSeparatee.BodyId.BodySource :?> Entity).GetPropPlus world).Prop.PropId)) field
                        field
                    else field
                just field

            | ScreenTransitioning transitioning ->
                let field = Field.updateScreenTransitioning (constant transitioning) field
                just field

            | MenuTeamOpen ->
                let state = MenuTeam { TeamIndex = 0; TeamIndices = Map.toKeyList field.Team }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = state }) field
                just field

            | MenuTeamAlly index ->
                let field =
                    Field.updateMenu (fun menu ->
                        let state =
                            match menu.MenuState with
                            | MenuTeam menuTeam -> MenuTeam { menuTeam with TeamIndex = index }
                            | state -> state
                        { menu with MenuState = state })
                        field
                just field

            | MenuInventoryOpen ->
                let inventoryState = MenuInventory { InventoryPage = 0 }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = inventoryState }) field
                just field

            | MenuInventoryPageUp ->
                let field =
                    Field.updateMenu (fun menu ->
                        match menu.MenuState with
                        | MenuInventory inventory -> { menu with MenuState = MenuInventory { InventoryPage = max 0 (dec inventory.InventoryPage) }}
                        | _ -> menu)
                        field
                just field

            | MenuInventoryPageDown ->
                let field =
                    Field.updateMenu (fun menu ->
                        match menu.MenuState with
                        | MenuInventory menuInventory -> { menu with MenuState = MenuInventory { InventoryPage = inc menuInventory.InventoryPage }}
                        | _ -> menu)
                        field
                just field

            | MenuInventorySelect (index, (itemType, _)) ->
                let field = Field.updateMenu (fun menu -> { menu with MenuUseOpt = MenuUse.tryMakeFromSelection (index, itemType) }) field
                just field

            | MenuInventoryUse index ->
                match Map.tryFind index field.Team with
                | Some teammate ->
                    match field.Menu.MenuUseOpt with
                    | Some menuUse ->
                        let itemType = snd menuUse.MenuUseSelection
                        let (result, displacedOpt, teammate) = Teammate.tryUseItem itemType teammate
                        let field = if result then Field.updateInventory (Inventory.tryRemoveItem itemType >> snd) field else field
                        let field = match displacedOpt with Some displaced -> Field.updateInventory (Inventory.tryAddItem displaced >> snd) field | None -> field
                        let field = Field.updateTeam (Map.add index teammate) field
                        let field = Field.updateMenu (constant { field.Menu with MenuUseOpt = None }) field
                        if result then withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.HealSound)) field
                        else just field
                    | None -> just field
                | None -> just field

            | MenuInventoryCancel ->
                let field = Field.updateMenu (fun menu -> { menu with MenuUseOpt = None }) field
                just field

            | MenuTechsOpen ->
                let state = MenuTechs { TeamIndex = 0 }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = state }) field
                just field
            
            | MenuTechsAlly index ->
                let field =
                    Field.updateMenu (fun menu ->
                        let state =
                            match menu.MenuState with
                            | MenuTechs menuTech -> MenuTechs { menuTech with TeamIndex = index }
                            | state -> state
                        { menu with MenuState = state })
                        field
                just field
            
            | MenuTechsSelect _ ->
                just field

            | MenuKeyItemsOpen ->
                let inventoryState = MenuKeyItems { KeyItemsPage = 0 }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = inventoryState }) field
                just field

            | MenuKeyItemsPageUp ->
                let field =
                    Field.updateMenu (fun menu ->
                        match menu.MenuState with
                        | MenuKeyItems menuKeyItems -> { menu with MenuState = MenuKeyItems { KeyItemsPage = max 0 (dec menuKeyItems.KeyItemsPage) }}
                        | _ -> menu)
                        field
                just field

            | MenuKeyItemsPageDown ->
                let field =
                    Field.updateMenu (fun menu ->
                        match menu.MenuState with
                        | MenuKeyItems menuKeyItems -> { menu with MenuState = MenuKeyItems { KeyItemsPage = inc menuKeyItems.KeyItemsPage }}
                        | _ -> menu)
                        field
                just field
            
            | MenuKeyItemsSelect _ ->
                just field

            | MenuOptionsOpen ->
                let state = MenuOptions
                let field = Field.updateMenu (fun menu -> { menu with MenuState = state }) field
                just field

            | MenuOptionsSelectBattleSpeed battleSpeed ->
                let field = Field.updateOptions (constant { BattleSpeed = battleSpeed }) field
                just field
            
            | MenuClose ->
                let field = Field.updateMenu (fun menu -> { menu with MenuState = MenuClosed }) field
                just field

            | ShopBuy ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopState = ShopBuying; ShopPage = 0 })) field
                just field

            | ShopSell ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopState = ShopSelling; ShopPage = 0 })) field
                just field

            | ShopPageUp ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopPage = max 0 (dec shop.ShopPage) })) field
                just field

            | ShopPageDown ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopPage = inc shop.ShopPage })) field
                just field

            | ShopSelect (index, (itemType, _)) ->
                let field =
                    Field.updateShopOpt (Option.map (fun shop ->
                        let buying = match shop.ShopState with ShopBuying -> true | ShopSelling -> false
                        let shopConfirmOpt = ShopConfirm.tryMakeFromSelection buying field.Inventory (index, itemType)
                        { shop with ShopConfirmOpt = shopConfirmOpt }))
                        field
                just field

            | ShopConfirmAccept ->
                match field.ShopOpt with
                | Some shop ->
                    match shop.ShopConfirmOpt with
                    | Some shopConfirm ->
                        let itemType = snd shopConfirm.ShopConfirmSelection
                        let valid =
                            match shop.ShopState with
                            | ShopBuying ->
                                Inventory.canAddItem itemType field.Inventory &&
                                field.Inventory.Gold >= shopConfirm.ShopConfirmPrice
                            | ShopSelling -> true
                        if valid then
                            let field = Field.updateInventory (match shop.ShopState with ShopBuying -> Inventory.tryAddItem itemType >> snd | ShopSelling -> Inventory.tryRemoveItem itemType >> snd) field
                            let field = Field.updateInventory (match shop.ShopState with ShopBuying -> Inventory.removeGold shopConfirm.ShopConfirmPrice | ShopSelling -> Inventory.addGold shopConfirm.ShopConfirmPrice) field
                            let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                            withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.PurchaseSound)) field
                        else withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.MistakeSound)) field
                    | None -> just field
                | None -> just field

            | ShopConfirmDecline ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                just field

            | ShopLeave ->
                let field = Field.updateShopOpt (constant None) field
                just field

            | PromptLeft ->
                match field.DialogOpt with
                | Some dialog ->
                    match dialog.DialogPromptOpt with
                    | Some ((_, promptCue), _) ->
                        let field = Field.updateDialogOpt (constant None) field
                        let field = Field.updateCue (constant promptCue) field
                        just field
                    | None -> just field
                | None -> just field

            | PromptRight ->
                match field.DialogOpt with
                | Some dialog ->
                    match dialog.DialogPromptOpt with
                    | Some (_, (_, promptCue)) ->
                        let field = Field.updateDialogOpt (constant None) field
                        let field = Field.updateCue (constant promptCue) field
                        just field
                    | None -> just field
                | None -> just field

            | TryBattle (battleType, consequents) ->
                match Map.tryFind battleType Data.Value.Battles with
                | Some battleData ->
                    let time = field.UpdateTime
                    let playTime = Option.defaultValue time field.FieldSongTimeOpt
                    let startTime = time - playTime
                    let prizePool = { Consequents = consequents; Items = []; Gold = 0; Exp = 0 }
                    let field = Field.enterBattle (World.getUpdateTime world) startTime prizePool battleData field
                    withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastGrowlSound)) field
                | None -> just field

            | Interact ->
                let (interactSignals, field) = Field.interact field
                let fieldSignals = List.map cast interactSignals
                let signals =
                    List.map (fun fieldSignal ->
                        match fieldSignal with
                        | FieldSignal.TryBattle (battleType, consequents) -> TryBattle (battleType, consequents) |> signal
                        | FieldSignal.PlaySound (delay, volume, sound) -> PlaySound (delay, volume, sound) |> signal
                        | FieldSignal.PlaySong (fadeInTime, fadeOutTime, songTime, volume, song) -> PlaySong (fadeInTime, fadeOutTime, songTime, volume, song) |> signal
                        | FieldSignal.FadeOutSong fadeOutTime -> FadeOutSong fadeOutTime |> signal)
                        fieldSignals
                (signals, field)

        override this.Command (field, command, screen, world) =

            match command with
            | ProcessKeyInput ->
                if  not (World.getSelectedScreenTransitioning world) &&
                    field.Menu.MenuState = MenuClosed &&
                    CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                    Option.isNone field.DialogOpt &&
                    Option.isNone field.ShopOpt &&
                    Option.isNone field.FieldTransitionOpt &&
                    Simulants.FieldSceneFeeler.GetTouched world |> not then
                    let force = v3Zero
                    let force = if World.isKeyboardKeyDown KeyboardKey.Right world || World.isKeyboardKeyDown KeyboardKey.D world then v3 Constants.Field.AvatarWalkForce 0.0f 0.0f + force else force
                    let force = if World.isKeyboardKeyDown KeyboardKey.Left world || World.isKeyboardKeyDown KeyboardKey.A world then v3 -Constants.Field.AvatarWalkForce 0.0f 0.0f + force else force
                    let force = if World.isKeyboardKeyDown KeyboardKey.Up world || World.isKeyboardKeyDown KeyboardKey.W world then v3 0.0f Constants.Field.AvatarWalkForce 0.0f + force else force
                    let force = if World.isKeyboardKeyDown KeyboardKey.Down world || World.isKeyboardKeyDown KeyboardKey.S world then v3 0.0f -Constants.Field.AvatarWalkForce 0.0f + force else force
                    let moveAvatar = MoveAvatar force
                    let directionOpt =
                        if World.isKeyboardKeyDown KeyboardKey.Right world || World.isKeyboardKeyDown KeyboardKey.D world then Some Rightward
                        elif World.isKeyboardKeyDown KeyboardKey.Left world || World.isKeyboardKeyDown KeyboardKey.A world then Some Leftward
                        elif World.isKeyboardKeyDown KeyboardKey.Up world || World.isKeyboardKeyDown KeyboardKey.W world then Some Upward
                        elif World.isKeyboardKeyDown KeyboardKey.Down world || World.isKeyboardKeyDown KeyboardKey.S world then Some Downward
                        else None
                    let faceAvatar =
                        match directionOpt with
                        | Some direction -> FaceAvatar direction
                        | None -> Nop
                    withSignals [moveAvatar; faceAvatar] world
                else just world

            | ProcessTouchInput position ->
                if  not (World.getSelectedScreenTransitioning world) &&
                    field.Menu.MenuState = MenuClosed &&
                    CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                    Option.isNone field.DialogOpt &&
                    Option.isNone field.ShopOpt &&
                    Option.isNone field.FieldTransitionOpt &&
                    World.isKeyboardKeyUp KeyboardKey.Right world && World.isKeyboardKeyUp KeyboardKey.D world &&
                    World.isKeyboardKeyUp KeyboardKey.Left world && World.isKeyboardKeyUp KeyboardKey.A world &&
                    World.isKeyboardKeyUp KeyboardKey.Up world && World.isKeyboardKeyUp KeyboardKey.W world &&
                    World.isKeyboardKeyUp KeyboardKey.Down world && World.isKeyboardKeyUp KeyboardKey.S world then
                    let lowerCenter = field.Avatar.LowerCenter
                    let viewport = World.getViewport world
                    let eyeCenter = World.getEyeCenter2d world
                    let eyeSize = World.getEyeSize2d world
                    let position = viewport.MouseToWorld2d (false, position, eyeCenter, eyeSize)
                    let heading = position.V3 - lowerCenter
                    if heading.Magnitude >= 6.0f then // TODO: make constant DeadZoneRadius.
                        let goalNormalized = Vector3.Normalize heading
                        let force = goalNormalized * Constants.Field.AvatarWalkForceMouse
                        let moveAvatar = MoveAvatar force
                        let faceAvatar = FaceAvatar (Direction.ofVector3 heading)
                        withSignals [moveAvatar; faceAvatar] world
                    else just world
                else just world

            | UpdateEye ->
                if world.Advancing then
                    let world = World.setEyeCenter2d field.Avatar.Center.V2 world
                    let tileMapPerimeter2d = (Simulants.FieldSceneTileMap.GetPerimeter world).Box2
                    let eyeBounds = tileMapPerimeter2d.WithMin (tileMapPerimeter2d.Min + v2 48.0f 48.0f)
                    let eyeBounds = eyeBounds.WithSize (tileMapPerimeter2d.Size - v2 96.0f 96.0f)
                    let world = World.constrainEyeBounds2d eyeBounds world
                    just world
                else just world

            | WarpAvatar bottom ->
                let bodyBottomOffset = v3Up * Constants.Gameplay.CharacterSize.Y * 0.5f
                let world = World.setBodyCenter (bottom + bodyBottomOffset) (Simulants.FieldSceneAvatar.GetBodyId world) world
                just world

            | MoveAvatar force ->
                let world =
                    if force <> v3Zero
                    then World.applyBodyForce force v3Zero (Simulants.FieldSceneAvatar.GetBodyId world) world
                    else world
                just world

            | FaceAvatar direction ->
                let linearVelocity = World.getBodyLinearVelocity (Simulants.FieldSceneAvatar.GetBodyId world) world
                let speed = linearVelocity.Magnitude
                let field =
                    if speed <= Constants.Field.AvatarIdleSpeedMax
                    then Field.updateAvatar (Avatar.updateDirection (constant direction)) field
                    else field
                let world = screen.SetField field world
                just world

            | PlayFieldSong ->
                match Data.Value.Fields.TryGetValue field.FieldType with
                | (true, fieldData) ->
                    match (fieldData.FieldSongOpt, World.getCurrentSongOpt world) with
                    | (Some fieldSong, Some currentSong) ->
                        if not (AssetTag.equals fieldSong currentSong.Song) then
                            let (playTime, startTime) =
                                let time = field.UpdateTime
                                match field.FieldSongTimeOpt with
                                | Some playTime ->
                                    let startTime = time - playTime
                                    if playTime < int64 Constants.Audio.SongResumptionMaximum
                                    then (playTime, startTime)
                                    else (0L, time)
                                | None -> (0L, time)
                            let fadeIn = if playTime <> 0L then Constants.Field.FieldSongFadeInTime else 0L
                            let field = Field.updateFieldSongTimeOpt (constant (Some startTime)) field
                            let world = screen.SetField field world
                            withSignal (PlaySong (fadeIn, 30L, playTime, Constants.Audio.SongVolumeDefault, fieldSong)) world
                        else just world
                    | (Some fieldSong, None) ->
                        let (playTime, startTime) =
                            let time = field.UpdateTime
                            match field.FieldSongTimeOpt with
                            | Some playTime ->
                                let startTime = time - playTime
                                if playTime < int64 Constants.Audio.SongResumptionMaximum
                                then (playTime, startTime)
                                else (0L, time)
                            | None -> (0L, time)
                        let fadeIn = if playTime <> 0L then Constants.Field.FieldSongFadeInTime else 0L
                        let field = Field.updateFieldSongTimeOpt (constant (Some startTime)) field
                        let world = screen.SetField field world
                        withSignal (PlaySong (fadeIn, 30L, playTime, Constants.Audio.SongVolumeDefault, fieldSong)) world
                    | (None, _) -> just world
                | (false, _) -> just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule delay (World.playSound volume sound) screen world
                just world

            | PlaySong (fadeIn, fadeOut, start, volume, assetTag) ->
                let world = World.playSong fadeIn fadeOut start volume assetTag world
                just world

            | FadeOutSong fade ->
                let world = World.fadeOutSong fade world
                just world

            | Nop -> just world

        override this.Content (field, _) =

            [// scene group
             Content.group Simulants.FieldScene.Name []

                [// avatar
                 Content.entity<AvatarDispatcher> Simulants.FieldSceneAvatar.Name
                    [Entity.Position == v3Zero
                     Entity.Elevation == Constants.Field.ForegroundElevation
                     Entity.Size == Constants.Gameplay.CharacterSize
                     Entity.Avatar := field.Avatar]

                 // props
                 for (index, prop) in field.Props.Pairs do
                    Content.entity<PropDispatcher> ("Prop+" + string index)
                        [Entity.PropPlus := PropPlus.make prop field.Advents field.Avatar.Bottom]

                 // spirit orb
                 if Field.hasEncounters field && CueSystem.Cue.isFin field.Cue then
                    Content.entity<SpiritOrbDispatcher> "SpiritOrb"
                        [Entity.Position == v3 -448.0f 48.0f 0.0f; Entity.Elevation == Constants.Field.SpiritOrbElevation; Entity.Size == v3 192.0f 192.0f 0.0f
                         Entity.SpiritOrb := { AvatarLowerCenter = field.Avatar.LowerCenter; ShowUnopenedChests = Field.getShowUnopenedChests field; Spirits = field.Spirits; Chests = Field.getChests field; Portals = Field.getNonWarpPortals field }]

                 // backdrop sprite
                 Content.staticSprite "Backdrop"
                    [Entity.Perimeter := field.ViewBoundsAbsolute.Box3; Entity.Elevation == Single.MinValue; Entity.Absolute == true
                     Entity.StaticImage == Assets.Default.White
                     Entity.Color :=
                        match Data.Value.Fields.TryGetValue field.FieldType with
                        | (true, fieldData) -> fieldData.FieldBackgroundColor
                        | (false, _) -> Color.Black]

                 // transition fade sprite
                 Content.staticSprite "Fade"
                    [Entity.Perimeter := field.ViewBoundsAbsolute.Box3; Entity.Elevation == Single.MaxValue; Entity.Absolute == true
                     Entity.StaticImage == Assets.Default.Black
                     Entity.Visible := Option.isSome field.FieldTransitionOpt
                     Entity.Color :=
                        match field.FieldTransitionOpt with
                        | Some transition ->
                            let time = field.UpdateTime
                            let localTime = single transition.FieldTransitionTime - single time
                            let halfTransitionTime = single Constants.Field.TransitionTime * 0.5f
                            let progress =
                                if localTime < halfTransitionTime
                                then localTime / halfTransitionTime
                                else 1.0f - (localTime - halfTransitionTime) / halfTransitionTime
                            Color.Black.WithA progress
                        | None -> Color.Zero]

                 // tmx map
                 Content.tmxMap Simulants.FieldSceneTileMap.Name
                    [Entity.Elevation == Constants.Field.BackgroundElevation
                     Entity.TmxMap :=
                        match Map.tryFind field.FieldType Data.Value.Fields with
                        | Some fieldData ->
                            match FieldData.tryGetTileMap field.OmniSeedState fieldData with
                            | Some tileMapChc ->
                                match tileMapChc with
                                | Choice1Of3 tileMap
                                | Choice2Of3 (tileMap, _)
                                | Choice3Of3 (tileMap, _) -> tileMap
                            | None -> failwithumf ()
                        | None -> failwithumf ()
                     Entity.TileIndexOffset :=
                         match Map.tryFind field.FieldType Data.Value.Fields with
                         | Some fieldData -> fieldData.FieldTileIndexOffset
                         | None -> failwithumf ()
                     Entity.TileIndexOffsetRange :=
                         match Map.tryFind field.FieldType Data.Value.Fields with
                         | Some fieldData -> fieldData.FieldTileIndexOffsetRange
                         | None -> failwithumf ()
                     Entity.TileLayerClearance == 10.0f]

                 // tmx map fade
                 Content.tmxMap "TileMapFade"
                    [Entity.Elevation == Constants.Field.BackgroundElevation + 0.5f
                     Entity.Color :=
                        (let progress = 1.0f - (Constants.Field.ConnectorFadeYMax - field.Avatar.Bottom.Y + Constants.Field.ConnectorFadeYMin) / Constants.Field.ConnectorFadeYMax
                         let fade = min 1.0f progress
                         Color.One.ScaleA fade)
                     Entity.TmxMap :=
                        match Map.tryFind field.FieldType Data.Value.Fields with
                        | Some fieldData ->
                           match FieldData.tryGetTileMap field.OmniSeedState fieldData with
                           | Some tileMapChc ->
                               match tileMapChc with
                               | Choice1Of3 _ -> Metadata.getTileMapMetadata Assets.Default.TileMapEmpty |> __c
                               | Choice2Of3 (_, tileMapFade) -> tileMapFade
                               | Choice3Of3 (_, _) ->  Metadata.getTileMapMetadata Assets.Default.TileMapEmpty |> __c
                           | None -> Metadata.getTileMapMetadata Assets.Default.TileMapEmpty |> __c
                        | None -> Metadata.getTileMapMetadata Assets.Default.TileMapEmpty |> __c
                     Entity.TileLayerClearance == 10.0f]

                 // feeler
                 Content.feeler Simulants.FieldSceneFeeler.Name
                    [Entity.Position == -Constants.Render.ResolutionF.V3 * 0.5f; Entity.Elevation == Constants.Field.FeelerElevation; Entity.Size == Constants.Render.ResolutionF.V3
                     Entity.TouchingEvent =|> fun evt -> ProcessTouchInput evt.Data |> signal]

                 // menu button
                 Content.button "Menu"
                    [Entity.Position == v3 -450.0f -246.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage
                     Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Text == "Menu"
                     Entity.Visible :=
                        field.Menu.MenuState = MenuClosed &&
                        CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt &&
                        not field.ScreenTransitioning
                     Entity.ClickEvent => MenuTeamOpen]

                 // party button
                 Content.button "Party"
                    [Entity.Position == v3 -72.0f -246.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage
                     Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible :=
                        field.Menu.MenuState = MenuClosed &&
                        (CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue || Option.isSome field.DialogOpt) &&
                        Option.isNone field.BattleOpt &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt &&
                        Field.touchingSavePoint field &&
                        not field.ScreenTransitioning &&
                        field.Team.Count > 2
                     Entity.Text == "Party"]

                 // interact button
                 Content.button "Interact"
                    [Entity.Position == v3 306.0f -246.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage
                     Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible :=
                        field.Menu.MenuState = MenuClosed &&
                        (CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue || Option.isSome field.DialogOpt) &&
                        Option.isNone field.BattleOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt &&
                        Option.isSome (Field.tryGetInteraction field) &&
                        not field.ScreenTransitioning
                     Entity.Text :=
                        match Field.tryGetInteraction field with
                        | Some interaction -> interaction
                        | None -> ""
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent => Interact]

                 // dialog
                 yield! Content.dialog "Dialog"
                    Constants.Field.GuiElevation PromptLeft PromptRight (Field.detokenize field)
                    (match field.DialogOpt with Some dialog -> Some dialog | None -> None)

                 // menu
                 match field.Menu.MenuState with
                 | MenuTeam menuTeam ->

                    // team
                    Content.panel "Team"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         yield! Content.team (v3 138.0f 417.0f 0.0f) Int32.MaxValue field (fun teammate menu ->
                            match menu.MenuState with
                            | MenuTeam team -> team.TeamIndex <> teammate.TeamIndex
                            | _ -> true)
                            MenuTeamAlly
                         Content.label "Portrait"
                            [Entity.PositionLocal == v3 438.0f 288.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 192.0f 0.0f
                             Entity.LabelImage :=
                                match MenuTeam.tryGetCharacterData field.Team menuTeam with
                                | Some characterData ->
                                    match characterData.PortraitOpt with
                                    | Some portrait -> portrait
                                    | None -> Assets.Default.ImageEmpty
                                | None -> Assets.Default.ImageEmpty]
                         Content.text "CharacterType"
                            [Entity.PositionLocal == v3 650.0f 372.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetCharacterData field.Team menuTeam with
                                | Some characterData -> CharacterType.getName characterData.CharacterType
                                | None -> ""]
                         Content.text "ArchetypeType"
                            [Entity.PositionLocal == v3 650.0f 336.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate -> string teammate.ArchetypeType + " Lv." + string (Algorithms.expPointsToLevel teammate.ExpPoints)
                                | None -> ""]
                         Content.text "Weapon"
                            [Entity.PositionLocal == v3 444.0f 237.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate -> "Wpn: " + Option.mapOrDefaultValue string "None" teammate.WeaponOpt
                                | None -> ""]
                         Content.text "Armor"
                            [Entity.PositionLocal == v3 444.0f 207.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate -> "Amr: " + Option.mapOrDefaultValue string "None" teammate.ArmorOpt
                                | None -> ""]
                         Content.text "Accessory"
                            [Entity.PositionLocal == v3 444.0f 177.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate -> "Acc: " + Option.mapOrDefaultValue string "None" (List.tryHead teammate.Accessories)
                                | None -> ""]
                         Content.text "Stats"
                            [Entity.PositionLocal == v3 444.0f -78.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 512.0f 256.0f 0.0f
                             Entity.Justification == Unjustified true
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate ->
                                    "HP  "   + (string teammate.HitPoints).PadLeft 3 +  " /" + (string teammate.HitPointsMax).PadLeft 3 +
                                    "\nTP  " + (string teammate.TechPoints).PadLeft 3 + " /" + (string teammate.TechPointsMax).PadLeft 3 +
                                    "\nPow " + (string teammate.Power).PadLeft 3 +      "   Mag " + (string $ teammate.Magic false).PadLeft 3 +
                                    "\nDef " + (string teammate.Defense).PadLeft 3 +    "   Abs " + (string teammate.Absorb).PadLeft 3 +
                                    "\nExp " + (string teammate.ExpPoints).PadLeft 3 +  " / " + string (Algorithms.expPointsForNextLevel teammate.ExpPoints)
                                | None -> ""]
                         Content.text "Gold"
                            [Entity.PositionLocal == v3 444.0f 9.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text := string field.Inventory.Gold + "G"]]

                 // inventory
                 | MenuInventory _ ->
                    Content.panel "Inventory"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage
                         Entity.Enabled := Option.isNone field.Menu.MenuUseOpt]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         yield! Content.items (v3 138.0f 417.0f 0.0f) 10 5 field MenuInventorySelect
                         Content.button "PageUp"
                            [Entity.PositionLocal == v3 138.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == "<"
                             Entity.VisibleLocal := Content.pageItems 10 field |> a__
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => MenuInventoryPageUp]
                         Content.button "PageDown"
                            [Entity.PositionLocal == v3 777.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == ">"
                             Entity.VisibleLocal := Content.pageItems 10 field |> _b_
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => MenuInventoryPageDown]]

                 // techs
                 | MenuTechs _ ->
                    Content.panel "Techs"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         yield! Content.team (v3 138.0f 417.0f 0.0f) Int32.MaxValue field (fun teammate menu ->
                            match menu.MenuState with
                            | MenuTechs techs -> techs.TeamIndex <> teammate.TeamIndex
                            | _ -> true)
                            MenuTechsAlly
                         yield! Content.techs (v3 513.0f 417.0f 0.0f) field MenuTechsSelect]

                 // key items
                 | MenuKeyItems _ ->
                    Content.panel "KeyItems"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage
                         Entity.Enabled := Option.isNone field.Menu.MenuUseOpt]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         yield! Content.items (v3 138.0f 417.0f 0.0f) 10 5 field MenuInventorySelect
                         Content.button "PageUp"
                            [Entity.PositionLocal == v3 138.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == "<"
                             Entity.VisibleLocal := Content.pageItems 10 field |> a__
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => MenuInventoryPageUp]
                         Content.button "PageDown"
                            [Entity.PositionLocal == v3 777.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == ">"
                             Entity.VisibleLocal := Content.pageItems 10 field |> _b_
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => MenuInventoryPageDown]]

                 // options
                 | MenuOptions ->
                    Content.panel "Options"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         Content.text "BattleSpeed"
                            [Entity.PositionLocal == v3 384.0f 432.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text == "Battle Speed"]
                         Content.radioButton "Wait"
                            [Entity.PositionLocal == v3 180.0f 372.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage
                             Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Wait"
                             Entity.Dialed := match field.Options.BattleSpeed with WaitSpeed -> true | _ -> false
                             Entity.DialedEvent => MenuOptionsSelectBattleSpeed WaitSpeed]
                         Content.radioButton "Paced"
                            [Entity.PositionLocal == v3 408.0f 372.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage
                             Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Paced"
                             Entity.Dialed := match field.Options.BattleSpeed with PacedSpeed -> true | _ -> false
                             Entity.DialedEvent => MenuOptionsSelectBattleSpeed PacedSpeed]
                         Content.radioButton "Swift"
                            [Entity.PositionLocal == v3 636.0f 372.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage
                             Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Swift"
                             Entity.Dialed := match field.Options.BattleSpeed with SwiftSpeed -> true | _ -> false
                             Entity.DialedEvent => MenuOptionsSelectBattleSpeed SwiftSpeed]]

                 // closed
                 | MenuClosed -> ()

                 // use
                 match field.Menu.MenuUseOpt with
                 | Some menuUse ->
                    Content.panel "Use"
                        [Entity.Position == v3 -450.0f -216.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v3 900.0f 432.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXLImage]
                        [yield! Content.team (v3 160.0f 183.0f 0.0f) 3 field (fun teammate menu ->
                            match menu.MenuUseOpt with
                            | Some menuUse -> Teammate.canUseItem (snd menuUse.MenuUseSelection) teammate
                            | None -> false)
                            MenuInventoryUse
                         Content.button "Close"
                            [Entity.PositionLocal == v3 810.0f 342.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.UpImage == asset "Field" "CloseButtonUp"
                             Entity.DownImage == asset "Field" "CloseButtonDown"
                             Entity.ClickEvent => MenuInventoryCancel]
                         Content.text "Line1"
                            [Entity.PositionLocal == v3 36.0f 354.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text := menuUse.MenuUseLine1]
                         Content.text "Line2"
                            [Entity.PositionLocal == v3 66.0f 312.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text := menuUse.MenuUseLine2]
                         Content.text "Line3"
                            [Entity.PositionLocal == v3 66.0f 270.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text := menuUse.MenuUseLine3]]
                 | None -> ()

                 // shop
                 match field.ShopOpt with
                 | Some shop ->
                    let items = Content.pageItems 8 field
                    Content.panel "Shop"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage
                         Entity.Enabled := Option.isNone shop.ShopConfirmOpt]
                        [yield! Content.items (v3 96.0f 347.0f 0.0f) 8 4 field ShopSelect
                         Content.button "Buy"
                            [Entity.PositionLocal == v3 24.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                             Entity.Text == "Buy"
                             Entity.VisibleLocal := shop.ShopState = ShopSelling
                             Entity.ClickEvent => ShopBuy]
                         Content.text "BuyWhat"
                            [Entity.PositionLocal == v3 24.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Buy what?"
                             Entity.VisibleLocal := shop.ShopState = ShopBuying]
                         Content.button "Sell"
                            [Entity.PositionLocal == v3 352.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                             Entity.Text == "Sell"
                             Entity.VisibleLocal := shop.ShopState = ShopBuying
                             Entity.ClickEvent => ShopSell]
                         Content.text "SellWhat"
                            [Entity.PositionLocal == v3 352.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Sell what?"
                             Entity.VisibleLocal := shop.ShopState = ShopSelling]
                         Content.button "Leave"
                            [Entity.PositionLocal == v3 678.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                             Entity.Text == "Leave"
                             Entity.ClickEvent => ShopLeave]
                         Content.button "PageUp"
                            [Entity.PositionLocal == v3 24.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == "<"
                             Entity.VisibleLocal := a__ items
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => ShopPageUp]
                         Content.button "PageDown"
                            [Entity.PositionLocal == v3 804.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == ">"
                             Entity.VisibleLocal := _b_ items
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => ShopPageDown]
                         Content.text "Gold"
                            [Entity.PositionLocal == v3 352.0f 3.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text := string field.Inventory.Gold + "G"]]
                 | None -> ()

                 // confirm
                 match field.ShopOpt with
                 | Some shop ->
                    match shop.ShopConfirmOpt with
                    | Some shopConfirm ->
                        Content.panel "Dialog"
                           [Entity.Position == v3 -450.0f -128.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v3 900.0f 252.0f 0.0f
                            Entity.LabelImage == Assets.Gui.DialogFatImage]
                           [Content.button "Accept"
                               [Entity.PositionLocal == v3 198.0f 36.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                                Entity.Text == "Accept"
                                Entity.ClickEvent => ShopConfirmAccept]
                            Content.button "Decline"
                               [Entity.PositionLocal == v3 498.0f 36.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                                Entity.Text == "Decline"
                                Entity.ClickEvent => ShopConfirmDecline]
                            Content.text "Offer"
                               [Entity.PositionLocal == v3 30.0f 180.0f 0.0f; Entity.ElevationLocal == 1.0f
                                Entity.Text := shopConfirm.ShopConfirmOffer]
                            Content.text "Line1"
                               [Entity.PositionLocal == v3 60.0f 138.0f 0.0f; Entity.ElevationLocal == 1.0f
                                Entity.Text := shopConfirm.ShopConfirmLine1]
                            Content.text "Line2"
                               [Entity.PositionLocal == v3 60.0f 96.0f 0.0f; Entity.ElevationLocal == 1.0f
                                Entity.Text := shopConfirm.ShopConfirmLine2]]
                    | None -> ()
                 | None -> ()]]