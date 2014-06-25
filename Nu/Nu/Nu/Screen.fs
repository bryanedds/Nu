namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module ScreenModule =

    type Screen with
        member this.Register (address : Address, groupDescriptors : GroupDescriptor list, world : World) : World = this?Register (address, groupDescriptors, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, world)

[<RequireQualifiedAccess>]
module Screen =

    let screenIncoming =
        { Get = fun screen -> screen.Incoming
          Set = fun value screen -> { screen with Incoming = value }}

    let screenOutgoing =
        { Get = fun screen -> screen.Outgoing
          Set = fun value screen -> { screen with Outgoing = value }}

    let makeDefault dispatcherName transitionDispatcherName =
        { Id = NuCore.getId ()
          State = IdlingState
          Incoming = Transition.makeDefault transitionDispatcherName Incoming
          Outgoing = Transition.makeDefault transitionDispatcherName Outgoing
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

    let makeDissolve dispatcherName transitionDispatcherName incomingTime outgoingTime =
        let optDissolveSprite = Some <| { SpriteAssetName = "Image8"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }
        let incomingDissolve = { Transition.makeDefault transitionDispatcherName Incoming with TransitionLifetime = incomingTime; OptDissolveSprite = optDissolveSprite }
        let outgoingDissolve = { Transition.makeDefault transitionDispatcherName Outgoing with TransitionLifetime = outgoingTime; OptDissolveSprite = optDissolveSprite }
        let screen = makeDefault dispatcherName transitionDispatcherName
        { screen with Incoming = incomingDissolve; Outgoing = outgoingDissolve }