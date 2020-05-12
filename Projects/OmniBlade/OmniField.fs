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

    type FieldMessage =
        | Nil

    type [<NoComparison>] FieldCommand =
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
             field.OutgoingStartEvent => [cmd FadeSong]]

        override this.Message (model, message, _, _) =

            match message with
            | Nil -> just model

        override this.Command (model, command, _, world) =

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
                     Entity.TileMapAsset <== model --> fun (model : FieldModel) ->
                        match Map.tryFind model.FieldType data.Value.Fields with
                        | Some fieldData -> fieldData.FieldTileMap
                        | None -> Assets.DebugRoomTileMap
                     Entity.TileLayerClearance == 10.0f]
                 Content.entity<AvatarDispatcher> Simulants.FieldAvatar.Name
                    [Entity.Size == Constants.Gameplay.CharacterSize
                     Entity.Position == v2 256.0f 256.0f
                     Entity.Depth == Constants.Field.ForgroundDepth]
                 //Content.entities
                 //   (model ->> fun model world ->
                 //       match Map.tryFind model.FieldType data.Value.Fields with
                 //       | Some fieldData ->
                 //           let (_, _, tileMap) = World.getTileMapMetadata fieldData.FieldTileMap world
                 //           let group = tileMap.ObjectGroups.Item "Props"
                 //           let objects = enumerable<TmxObjectGroup.TmxObject> group.Objects
                 //           List.ofSeq objects
                 //       | None -> [])
                 //   (fun _ lens world ->
                 //       let object = lens.Get world
                 //       Content.entity<PropDispatcher> object.)
                        ]]