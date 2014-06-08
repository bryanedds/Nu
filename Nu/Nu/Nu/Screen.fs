namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuCore
open Nu.NuConstants
open Nu.Sim
open Nu.Group

[<AutoOpen>]
module ScreenModule =

    type Transition with
        end

    type Screen with
        member this.Register (address : Address, groupDescriptors : GroupDescriptor list, world : World) : World = this?Register (address, groupDescriptors, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, world)

module Screen =

    let screenIncoming =
        { Get = fun screen -> screen.Incoming
          Set = fun value screen -> { screen with Incoming = value }}

    let screenOutgoing =
        { Get = fun screen -> screen.Outgoing
          Set = fun value screen -> { screen with Outgoing = value }}

    let makeDissolveSprite () =
        { SpriteAssetName = "Image8"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

    let makeDefaultTransition defaultDispatcherName transitionType =
        { Id = getNuId ()
          Lifetime = 0
          Ticks = 0
          Type = transitionType
          OptDissolveSprite = None
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some defaultDispatcherName; CanDefault = true; Sealed = false }}

    let makeDefaultScreen defaultDispatcherName defaultTransitionDispatcherName =
        { Id = getNuId ()
          State = IdlingState
          Incoming = makeDefaultTransition defaultTransitionDispatcherName Incoming
          Outgoing = makeDefaultTransition defaultTransitionDispatcherName Outgoing
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some defaultDispatcherName; CanDefault = true; Sealed = false }}

    let makeDissolveScreen dispatcherName defaultTransitionDispatcherName incomingTime outgoingTime =
        let optDissolveSprite = Some <| makeDissolveSprite ()
        let incomingDissolve = { makeDefaultTransition defaultTransitionDispatcherName Incoming with Lifetime = incomingTime; OptDissolveSprite = optDissolveSprite }
        let outgoingDissolve = { makeDefaultTransition defaultTransitionDispatcherName Outgoing with Lifetime = outgoingTime; OptDissolveSprite = optDissolveSprite }
        let screen = makeDefaultScreen dispatcherName defaultTransitionDispatcherName
        { screen with Incoming = incomingDissolve; Outgoing = outgoingDissolve }