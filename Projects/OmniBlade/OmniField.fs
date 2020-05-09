namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniField =

    type FieldMessage =
        | Nil

    type [<NoComparison>] FieldCommand =
        | FadeSong
        | PlaySound of int64 * single * AssetTag<Audio>

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
            [field.OutgoingStartEvent => [cmd FadeSong]]

        override this.Message (model, message, _, _) =

            match message with
            | Nil -> just model

        override this.Command (model, command, battle, world) =

            match command with
            | FadeSong ->
                let world = World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
                just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

        override this.Content (model, screen, world) =
            []