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

    type FieldCommand =
        | FadeSong
        | PlaySound of int64 * single * AssetTag<Audio>
        | Move of Vector2
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

        override this.Channel (_, field, _) =
            [field.UpdateEvent =|> fun _ ->
                let force = v2Zero
                let force = if KeyboardState.isKeyDown KeyboardKey.Right then v2 Constants.Field.WalkForce 0.0f + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Left then v2 -Constants.Field.WalkForce 0.0f + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Up then v2 0.0f Constants.Field.WalkForce + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Down then v2 0.0f -Constants.Field.WalkForce + force else force
                [cmd (Move force)]
             field.PostUpdateEvent => [cmd EyeTrack]
             field.OutgoingStartEvent => [cmd FadeSong]
             Simulants.FieldAvatar.AvatarModel.ChangeEvent =|> fun evt ->
                let avatarModel = evt.Data.Value :?> AvatarModel
                [msg (AvatarChanged avatarModel)]]

        override this.Message (model, message, _, _) =

            match message with
            | AvatarChanged avatarModel ->
                let model = FieldModel.updateAvatar (constant avatarModel) model
                just model

        override this.Command (_, command, _, world) =

            match command with
            | Move force ->
                let physicsId = Simulants.FieldAvatar.GetPhysicsId world
                let world = World.applyBodyForce force physicsId world
                just world

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

        override this.Content (model, _, _) =
            [Content.layer Simulants.FieldScene.Name []
                [Content.tileMap Simulants.FieldTileMap.Name
                    [Entity.Depth == Constants.Field.BackgroundDepth
                     Entity.TileMapAsset <== model --> fun model ->
                        match Map.tryFind model.FieldType data.Value.Fields with
                        | Some fieldData -> fieldData.FieldTileMap
                        | None -> Assets.DebugRoomTileMap
                     Entity.TileLayerClearance == 10.0f]
                 Content.entity<AvatarDispatcher> Simulants.FieldAvatar.Name
                    [Entity.Size == Constants.Gameplay.CharacterSize
                     Entity.Position == v2 256.0f 256.0f
                     Entity.Depth == Constants.Field.ForgroundDepth
                     Entity.LinearDamping == Constants.Field.LinearDamping
                     Entity.AvatarModel <== model --> fun model -> model.Avatar]
                 Content.button Simulants.FieldInteract.Name
                    [Entity.Size == v2Dup 92.0f
                     Entity.Position == v2 -400.0f -200.0f
                     Entity.Depth == Constants.Field.GuiDepth
                     Entity.Enabled <== model --> fun model -> List.notEmpty model.Avatar.IntersectedBodyShapes
                     Entity.Text == "XXX"]
                 Content.entities
                    (model ->> fun model world ->
                        match Map.tryFind model.FieldType data.Value.Fields with
                        | Some fieldData ->
                            match World.tryGetTileMapMetadata fieldData.FieldTileMap world with
                            | Some (_, _, tileMap) ->
                                if tileMap.ObjectGroups.Contains Constants.Field.PropsLayerName then
                                    let group = tileMap.ObjectGroups.Item Constants.Field.PropsLayerName
                                    let objects = enumerable<TmxObject> group.Objects
                                    let objectsAndGroups = Seq.map (fun object -> (object, group, tileMap)) objects
                                    Seq.eval objectsAndGroups
                                else Seq.empty
                            | None -> Seq.empty
                        | None -> Seq.empty)
                    (fun _ lens world ->
                        let (object, group, tileMap) = lens.Get world
                        let propPosition = v2 (single object.X) (single tileMap.Height * single tileMap.TileHeight - single object.Y) // invert y
                        let propBounds = v4Bounds propPosition Constants.Gameplay.TileSize
                        let propDepth =
                            match group.Properties.TryGetValue Constants.Physics.DepthProperty with
                            | (true, depth) -> Constants.Field.ForgroundDepth + scvalue depth
                            | (false, _) -> Constants.Field.ForgroundDepth
                        let propData = scvalue<PropData> object.Type
                        let propModel = PropModel.make propBounds propDepth propData
                        Content.entity<PropDispatcher> object.Name [Entity.PropModel == propModel])]]