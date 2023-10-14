namespace Tactics
open System
open Prime
open Nu

[<AutoOpen>]
module AtlasDispatcher =

    type AtlasMessage =
        | UpdateMessage
        interface Message

    type AtlasCommand =
        | UpdateCommand
        interface Command

    type Screen with
        member this.GetAtlas world = this.GetModelGeneric<Atlas> world
        member this.SetAtlas value world = this.SetModelGeneric<Atlas> value world
        member this.Atlas = this.ModelGeneric<Atlas> ()

    type AtlasDispatcher () =
        inherit ScreenDispatcher<Atlas, AtlasMessage, AtlasCommand> (Atlas.empty)