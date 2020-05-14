namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open TiledSharp
open OmniBlade

[<AutoOpen>]
module OmniField =

    type [<NoComparison>] FieldMessage =
        | AvatarChanged of AvatarModel
        | UpdateDialog
        | Interact

    type FieldCommand =
        | FadeSong
        | PlaySound of int64 * single * AssetTag<Audio>
        | TryMove of Vector2
        | EyeTrack

    type Screen with

        member this.GetFieldModel = this.GetModel<FieldModel>
        member this.SetFieldModel = this.SetModel<FieldModel>
        member this.FieldModel = this.Model<FieldModel> ()

    type FieldDispatcher () =
        inherit ScreenDispatcher<FieldModel, FieldMessage, FieldCommand>
            (FieldModel.make
                DebugRoom
                (AvatarModel.make (v4Bounds (v2 128.0f 128.0f) Constants.Gameplay.CharacterSize) Assets.FinnAnimationSheet Downward)
                Map.empty
                Set.empty
                { Items = Map.empty }
                100)

        static let tryGetInteraction dialogOpt advents prop =
            match dialogOpt with
            | Some dialog ->
                if dialog.DialogProgress > String.Join("\n", dialog.DialogText).Length
                then Some "Next"
                else None
            | None ->
                match prop with
                | Chest (_, _, _, chestId) ->
                    if Set.contains (Opened chestId) advents then None
                    else Some "Open"
                | Door _ -> Some "Open"
                | Portal -> None
                | Switch -> Some "Use"
                | Sensor -> None
                | Npc _ -> Some "Talk"
                | Shopkeep _ -> Some "Inquire"

        static let isFacingBodyShape bodyShape (avatar : AvatarModel) world =
            if bodyShape.SourceEntity.Is<PropDispatcher> world then
                let v = bodyShape.SourceEntity.GetBottom world - avatar.Bottom
                let direction = Direction.fromVector2 v
                direction = avatar.Direction
            else false

        static let getFacingBodyShapes (avatar : AvatarModel) world =
            List.filter
                (fun bodyShape -> isFacingBodyShape bodyShape avatar world)
                avatar.IntersectedBodyShapes

        static let tryGetFacingProp (avatar : AvatarModel) world =
            match getFacingBodyShapes avatar world with
            | head :: _ ->
                // TODO: distance-sort these instead of just taking head
                let prop = head.SourceEntity.GetPropModel world
                Some prop.PropData
            | [] -> None

        static let tryGetInteraction dialogOpt advents (avatar : AvatarModel) world =
            match tryGetFacingProp avatar world with
            | Some prop -> tryGetInteraction dialogOpt advents prop
            | None -> None

        override this.Channel (_, field, _) =
            [field.UpdateEvent =|> fun _ ->
                let force = v2Zero
                let force = if KeyboardState.isKeyDown KeyboardKey.Right then v2 Constants.Field.WalkForce 0.0f + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Left then v2 -Constants.Field.WalkForce 0.0f + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Up then v2 0.0f Constants.Field.WalkForce + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Down then v2 0.0f -Constants.Field.WalkForce + force else force
                [msg UpdateDialog; cmd (TryMove force)]
             field.PostUpdateEvent => [cmd EyeTrack]
             field.OutgoingStartEvent => [cmd FadeSong]
             Simulants.FieldInteract.ClickEvent => [msg Interact]
             Simulants.FieldAvatar.AvatarModel.ChangeEvent =|> fun evt -> [msg (AvatarChanged (evt.Data.Value :?> AvatarModel))]]

        override this.Message (model, message, _, world) =

            match message with
            | AvatarChanged avatarModel ->
                let model = FieldModel.updateAvatar (constant avatarModel) model
                just model

            | UpdateDialog ->
                let model =
                    FieldModel.updateDialogOpt
                        (function
                         | Some dialog ->
                            let increment = if World.getTickTime world % 3L = 0L then 1 else 0
                            Some { dialog with DialogProgress = dialog.DialogProgress + increment }
                         | None -> None)
                        model
                just model

            | Interact ->
                match model.DialogOpt with
                | Some _ ->
                    let model = FieldModel.updateDialogOpt (constant None) model
                    just model
                | None ->
                    match tryGetFacingProp model.Avatar world with
                    | Some prop ->
                        match prop with
                        | Chest (itemType, lockType, chestType, chestId) ->
                            let model = FieldModel.updateInventory (Inventory.addItem itemType) model
                            let model = FieldModel.updateAdvents (Set.add (Opened chestId)) model
                            let model = FieldModel.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = ["Found " + ItemType.getName itemType + "!"]; DialogProgress = 0 })) model
                            withCmd model (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.OpenChestSound))
                        | _ -> just model
                    | None -> just model

        override this.Command (model, command, _, world) =

            match command with
            | TryMove force ->
                if Option.isNone model.DialogOpt then
                    let physicsId = Simulants.FieldAvatar.GetPhysicsId world
                    let world = World.applyBodyForce force physicsId world
                    just world
                else just world

            | EyeTrack ->
                let avatarModel = Simulants.FieldAvatar.GetAvatarModel world
                let world = World.setEyeCenter avatarModel.Center world
                just world

            | FadeSong ->
                let world = World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
                just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

        override this.Content (model, _) =
            
            // main layer
            [Content.layer Simulants.FieldScene.Name []
                
                // tile map
                [Content.tileMap Simulants.FieldTileMap.Name
                    [Entity.Depth == Constants.Field.BackgroundDepth
                     Entity.TileMapAsset <== model --> fun model ->
                        match Map.tryFind model.FieldType data.Value.Fields with
                        | Some fieldData -> fieldData.FieldTileMap
                        | None -> Assets.DebugRoomTileMap
                     Entity.TileLayerClearance == 10.0f]
                 
                 // avatar
                 Content.entity<AvatarDispatcher> Simulants.FieldAvatar.Name
                    [Entity.Size == Constants.Gameplay.CharacterSize
                     Entity.Position == v2 256.0f 256.0f
                     Entity.Depth == Constants.Field.ForgroundDepth
                     Entity.LinearDamping == Constants.Field.LinearDamping
                     Entity.AvatarModel <== model --> fun model -> model.Avatar]
                 
                 // interact button
                 Content.button Simulants.FieldInteract.Name
                    [Entity.Size == v2Dup 92.0f
                     Entity.Position == v2 360.0f 160.0f
                     Entity.Depth == Constants.Field.GuiDepth
                     Entity.Visible <== model ->> fun model world ->
                        let interactionOpt = tryGetInteraction model.DialogOpt model.Advents model.Avatar world
                        Option.isSome interactionOpt
                     Entity.Text <== model ->> fun model world ->
                        match tryGetInteraction model.DialogOpt model.Advents model.Avatar world with
                        | Some interaction -> interaction
                        | None -> ""
                     Entity.ClickSoundOpt == None]
                 
                 // dialog
                 Content.text Simulants.FieldDialog.Name
                    [Entity.Bounds <== model --> fun model ->
                        match model.DialogOpt with
                        | Some dialog ->
                            match dialog.DialogForm with
                            | DialogThin -> v4Bounds (v2 -448.0f 128.0f) (v2 896.0f 112.0f)
                            | DialogMedium -> v4Bounds (v2 -448.0f 128.0f) (v2 640.0f 320.0f)
                            | DialogLarge -> v4Bounds (v2 -448.0f 128.0f) (v2 896.0f 320.0f)
                        | None -> v4Zero
                     Entity.BackgroundImage <== model --> fun model ->
                        match model.DialogOpt with
                        | Some dialog ->
                            match dialog.DialogForm with
                            | DialogThin -> Assets.DialogThin
                            | DialogMedium -> Assets.DialogMedium
                            | DialogLarge -> Assets.DialogLarge
                        | None -> Assets.DialogLarge
                     Entity.Text <== model --> fun model ->
                        match model.DialogOpt with
                        | Some dialog ->
                            let text = String.Join ("\n", dialog.DialogText)
                            let textToShow = String.tryTake dialog.DialogProgress text
                            textToShow
                        | None -> ""
                     Entity.Visible <== model --> fun model -> Option.isSome model.DialogOpt
                     Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                     Entity.Margins == v2 32.0f 0.0f]
                 
                 // props
                 Content.entities
                    (model ->> fun model world ->
                        match Map.tryFind model.FieldType data.Value.Fields with
                        | Some fieldData ->
                            match World.tryGetTileMapMetadata fieldData.FieldTileMap world with
                            | Some (_, _, tileMap) ->
                                if tileMap.ObjectGroups.Contains Constants.Field.PropsLayerName then
                                    let group = tileMap.ObjectGroups.Item Constants.Field.PropsLayerName
                                    let objects = enumerable<TmxObject> group.Objects
                                    let results = Seq.map (fun object -> (object, group, tileMap, model.Advents)) objects
                                    Seq.toList results
                                else []
                            | None -> []
                        | None -> [])
                    (fun _ model _ ->
                        let propModel = model.Map (fun (object, group, tileMap, advents) ->
                            let propPosition = v2 (single object.X) (single tileMap.Height * single tileMap.TileHeight - single object.Y) // invert y
                            let propBounds = v4Bounds propPosition Constants.Gameplay.TileSize
                            let propDepth =
                                match group.Properties.TryGetValue Constants.TileMap.DepthPropertyName with
                                | (true, depthStr) -> Constants.Field.ForgroundDepth + scvalue depthStr
                                | (false, _) -> Constants.Field.ForgroundDepth
                            let propData =
                                match object.Properties.TryGetValue Constants.TileMap.InfoPropertyName with
                                | (true, propDataStr) -> scvalue<PropData> propDataStr
                                | (false, _) -> PropData.empty
                            let propModel = PropModel.make propBounds propDepth advents propData
                            propModel)
                        Content.entity<PropDispatcher> Gen.name [Entity.PropModel <== propModel])]]