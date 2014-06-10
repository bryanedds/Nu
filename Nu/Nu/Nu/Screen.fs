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

    let makeDefault defaultDispatcherName defaultTransitionDispatcherName =
        { Id = NuCore.getId ()
          State = IdlingState
          Incoming = Transition.makeDefault defaultTransitionDispatcherName Incoming
          Outgoing = Transition.makeDefault defaultTransitionDispatcherName Outgoing
          FacetNamesNs = []
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some defaultDispatcherName; CanDefault = true; Sealed = false }}

    let makeDissolve dispatcherName defaultTransitionDispatcherName incomingTime outgoingTime =
        let optDissolveSprite = Some <| { SpriteAssetName = "Image8"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }
        let incomingDissolve = { Transition.makeDefault defaultTransitionDispatcherName Incoming with Lifetime = incomingTime; OptDissolveSprite = optDissolveSprite }
        let outgoingDissolve = { Transition.makeDefault defaultTransitionDispatcherName Outgoing with Lifetime = outgoingTime; OptDissolveSprite = optDissolveSprite }
        let screen = makeDefault dispatcherName defaultTransitionDispatcherName
        { screen with Incoming = incomingDissolve; Outgoing = outgoingDissolve }