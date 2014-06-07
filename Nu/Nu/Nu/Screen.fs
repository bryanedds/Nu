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

    type TransitionDispatcher () =
        class end

    type ScreenDispatcher () =

        abstract member Register : Screen * Address * GroupDescriptor list * World -> World
        default this.Register (_, address, groupDescriptors, world) = addGroups address groupDescriptors world

        abstract member Unregister : Screen * Address * World -> World
        default this.Unregister (_, address, world) = removeGroups address world

module Screen =

    let transitionId =
        { Get = fun (transition : Transition) -> transition.Id
          Set = fun value transition -> { transition with Id = value }}

    let transitionLifetime =
        { Get = fun transition -> transition.Lifetime
          Set = fun value transition -> { transition with Lifetime = value }}

    let transitionTicks =
        { Get = fun transition -> transition.Ticks
          Set = fun value transition -> { transition with Ticks = value }}

    let transitionType =
        { Get = fun transition -> transition.Type
          Set = fun value transition -> { transition with Type = value }}

    let transitionXtension =
        { Get = fun (transition : Transition) -> transition.Xtension
          Set = fun value transition -> { transition with Xtension = value }}

    let transitionXField fieldName =
        { Get = fun (transition : Transition) -> (?) transition fieldName
          Set = fun value transition -> (?<-) transition fieldName value }

    let screenId =
        { Get = fun (screen : Screen) -> screen.Id
          Set = fun value screen -> { screen with Id = value }}

    let screenState =
        { Get = fun screen -> screen.State
          Set = fun value screen -> { screen with State = value }}

    let screenIncoming =
        { Get = fun screen -> screen.Incoming
          Set = fun value screen -> { screen with Incoming = value }}

    let screenOutgoing =
        { Get = fun screen -> screen.Outgoing
          Set = fun value screen -> { screen with Outgoing = value }}

    let screenXtension =
        { Get = fun (screen : Screen) -> screen.Xtension
          Set = fun value screen -> { screen with Xtension = value }}

    let screenXField fieldName =
        { Get = fun (screen : Screen) -> (?) screen fieldName
          Set = fun value screen -> (?<-) screen fieldName value }

    let private worldOptScreenFinder (address : Address) world =
        Map.tryFind (List.at 0 address) world.Screens

    let private worldScreenAdder (address : Address) world child =
        { world with Screens = Map.add (List.at 0 address) child world.Screens }

    let private worldScreenRemover (address : Address) world =
        { world with Screens = Map.remove (List.at 0 address) world.Screens }

    let worldScreen address =
        { Get = fun world -> Option.get <| worldOptScreenFinder address world
          Set = fun screen world -> worldScreenAdder address world screen }

    let worldOptScreen address =
        { Get = fun world -> worldOptScreenFinder address world
          Set = fun optScreen world -> match optScreen with None -> worldScreenRemover address world | Some screen -> worldScreenAdder address world screen }

    let worldScreens address =
        { Get = fun world ->
            match address with
            | [] -> world.Screens
            | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'."
          Set = fun screens world ->
            match address with
            | [] -> { world with Screens = Map.addMany (Map.toSeq screens) world.Screens }
            | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'." }

    let worldScreenIncoming address = worldScreen address >>| screenIncoming
    let worldScreenOutgoing address = worldScreen address >>| screenOutgoing

    let withWorldScreen fn address world = withWorldSimulant worldScreen
    let withWorldOptScreen fn address world = withWorldOptSimulant worldOptScreen
    let tryWithWorldScreen fn address world = tryWithWorldSimulant worldOptScreen worldScreen
    
    let makeDissolveSprite () =
        { SpriteAssetName = "Image8"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

    let makeDefaultTransition transitionType =
        { Id = getNuId ()
          Lifetime = 0
          Ticks = 0
          Type = transitionType
          OptDissolveSprite = None
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some typeof<TransitionDispatcher>.Name; CanDefault = true; Sealed = false }}

    let makeDefaultScreen () =
        { Id = getNuId ()
          State = IdlingState
          Incoming = makeDefaultTransition Incoming
          Outgoing = makeDefaultTransition Outgoing
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some typeof<ScreenDispatcher>.Name; CanDefault = true; Sealed = false }}

    let makeDissolveScreen screenDispatcherName incomingTime outgoingTime =
        let optDissolveSprite = Some <| makeDissolveSprite ()
        let incomingDissolve = { makeDefaultTransition Incoming with Lifetime = incomingTime; OptDissolveSprite = optDissolveSprite }
        let outgoingDissolve = { makeDefaultTransition Outgoing with Lifetime = outgoingTime; OptDissolveSprite = optDissolveSprite }
        let screen = makeDefaultScreen () 
        { screen with
            Incoming = incomingDissolve
            Outgoing = outgoingDissolve
            Xtension = { screen.Xtension with OptXDispatcherName = Some screenDispatcherName }}

    let registerScreen address (screen : Screen) groupDescriptors world =
        screen.Register (address, groupDescriptors, world)

    let unregisterScreen address world =
        let screen = get world <| worldScreen address
        screen.Unregister (address, world)

    let removeScreen address world =
        let world' = unregisterScreen address world
        set None world' <| worldOptScreen address

    let addScreen address screen groupDescriptors world =
        let world' =
            match get world <| worldOptScreen address with
            | None -> world
            | Some _ -> removeScreen address world
        let world'' = registerScreen address screen groupDescriptors world'
        set screen world'' <| worldScreen address