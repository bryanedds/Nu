namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuCore
open Nu.NuConstants
open Nu.DomainModel
open Nu.Group

[<AutoOpen>]
module XScreen =

    type GroupDescriptor = Lun * Group * Entity list

    type Transition with end

    type Screen with
        member this.Register (address : Address, groupDescriptors : GroupDescriptor list, world : World) : World = this?Register (address, this, groupDescriptors, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, this, world)

[<AutoOpen>]
module ScreenModule =

    type TransitionDispatcher () = class end

    type ScreenDispatcher () =

        abstract member Register : Address * Screen * GroupDescriptor list * World -> World
        default this.Register (address, _, groupDescriptors, world) = addGroups address groupDescriptors world

        abstract member Unregister : Address * Screen * World -> World
        default this.Unregister (address, _, world) = removeGroups address world

module Screen =

    let transitionIdLens =
        { Get = fun (transition : Transition) -> transition.Id
          Set = fun value transition -> { transition with Id = value }}

    let transitionLifetimeLens =
        { Get = fun transition -> transition.Lifetime
          Set = fun value transition -> { transition with Lifetime = value }}

    let transitionTicksLens =
        { Get = fun transition -> transition.Ticks
          Set = fun value transition -> { transition with Ticks = value }}

    let transitionTypeLens =
        { Get = fun transition -> transition.Type
          Set = fun value transition -> { transition with Type = value }}

    let transitionXtensionLens =
        { Get = fun (transition : Transition) -> transition.Xtension
          Set = fun value transition -> { transition with Xtension = value }}

    let transitionDynamicLens memberName =
        { Get = fun (transition : Transition) -> (?) transition memberName
          Set = fun value transition -> (?<-) transition memberName value }

    let screenIdLens =
        { Get = fun (screen : Screen) -> screen.Id
          Set = fun value screen -> { screen with Id = value }}

    let screenStateLens =
        { Get = fun screen -> screen.State
          Set = fun value screen -> { screen with State = value }}

    let screenIncomingLens =
        { Get = fun screen -> screen.Incoming
          Set = fun value screen -> { screen with Incoming = value }}

    let screenOutgoingLens =
        { Get = fun screen -> screen.Outgoing
          Set = fun value screen -> { screen with Outgoing = value }}

    let screenXtensionLens =
        { Get = fun (screen : Screen) -> screen.Xtension
          Set = fun value screen -> { screen with Xtension = value }}

    let screenDynamicLens memberName =
        { Get = fun (screen : Screen) -> (?) screen memberName
          Set = fun value screen -> (?<-) screen memberName value }

    let worldOptScreenFinder address world =
        Map.tryFind (List.at 0 address) world.Screens

    let worldScreenAdder address world child =
        { world with Screens = Map.add (List.at 0 address) child world.Screens }

    let worldScreenRemover address world =
        { world with Screens = Map.remove (List.at 0 address) world.Screens }

    let worldScreenLens address =
        { Get = fun world -> Option.get <| worldOptScreenFinder address world
          Set = fun screen world -> worldScreenAdder address world screen }

    let worldOptScreenLens address =
        { Get = fun world -> worldOptScreenFinder address world
          Set = fun optScreen world -> match optScreen with None -> worldScreenRemover address world | Some screen -> worldScreenAdder address world screen }

    let worldScreensLens address =
        { Get = fun world ->
            match address with
            | [] -> world.Screens
            | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'."
          Set = fun screens world ->
            match address with
            | [] -> { world with Screens = Map.addMany (Map.toSeq screens) world.Screens }
            | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'." }

    let worldScreenIncomingLens address = worldScreenLens address >>| screenIncomingLens
    let worldScreenOutgoingLens address = worldScreenLens address >>| screenOutgoingLens
    
    let makeDissolveSprite () =
        { SpriteAssetName = Lun.make "Image8"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

    let makeDefaultTransition transitionType =
        { Id = getNuId ()
          Lifetime = 0
          Ticks = 0
          Type = transitionType
          OptDissolveSprite = None
          Xtension = { OptXTypeName = Some <| Lun.make typeof<TransitionDispatcher>.Name; XFields = Map.empty }}

    let makeDefaultScreen () =
        { Id = getNuId ()
          State = IdlingState
          Incoming = makeDefaultTransition Incoming
          Outgoing = makeDefaultTransition Outgoing
          Xtension = { OptXTypeName = Some <| Lun.make typeof<ScreenDispatcher>.Name; XFields = Map.empty }}

    let makeDissolveScreen incomingTime outgoingTime =
        let optDissolveSprite = Some <| makeDissolveSprite ()
        let incomingDissolve = { makeDefaultTransition Incoming with Lifetime = incomingTime; OptDissolveSprite = optDissolveSprite }
        let outgoingDissolve = { makeDefaultTransition Outgoing with Lifetime = outgoingTime; OptDissolveSprite = optDissolveSprite  }
        { makeDefaultScreen () with Incoming = incomingDissolve; Outgoing = outgoingDissolve }

    let registerScreen address (screen : Screen) groupDescriptors world =
        screen.Register (address, groupDescriptors, world)

    let unregisterScreen address world =
        let screen = get world <| worldScreenLens address
        screen.Unregister (address, world)

    let removeScreen address world =
        let world' = unregisterScreen address world
        set None world' (worldOptScreenLens address)

    let addScreen address screen groupDescriptors world =
        let world' =
            match get world <| worldOptScreenLens address with
            | None -> world
            | Some _ -> removeScreen address world
        let world'' = registerScreen address screen groupDescriptors world'
        set screen world'' (worldScreenLens address)