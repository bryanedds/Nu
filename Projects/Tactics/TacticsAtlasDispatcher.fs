namespace Tactics
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module AtlasDispatcher =

    type AtlasMessage =
        | UpdateMessage

    type AtlasCommand =
        | UpdateCommand

    type Screen with
        member this.GetAtlas world = this.GetModelGeneric<Atlas> world
        member this.SetAtlas value world = this.SetModelGeneric<Atlas> value world
        member this.Atlas = this.ModelGeneric<Atlas> ()

    type AtlasDispatcher () =
        inherit ScreenDispatcher<Atlas, AtlasMessage, AtlasCommand> (Atlas.initial Slot1)